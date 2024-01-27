;; fennel-ls: macro-file
"Macros to define checks."

(λ defcheck [check-table check-type apply? name enabled? param docstring & body]
  "Generic macro to define a check.
   `check-table`: place the generated check in this table
   `check-type`: `:ast` or `:line`
   `apply?`: this is a predicate that receives an ast node andreturns whether
             the check is appplicable to the node.
   `name`: name of the check
   `enabled?`: whether the check is enabled by default
   `param`: function parameter table
   `docstring`: function/check docstring
   `body`: function body"
  (let [config ((. (require :config) :get))
        default? enabled?
        enabled? (if (not= nil (?. config :checks name))
                   (?. config :checks name)
                   default?)]
    `(tset
      ,check-table
      ,name
      {:docstring ,docstring
       :default? ,default?
       :enabled? ,enabled?
       :type ,check-type
       :apply? ,apply?
       :fn (fn ,param ,docstring ,(unpack body))}))) ; no-check

(λ list= [ast schema]
  "Checks if `ast` matches `schema`."
  (let [result `(and (= (length ,ast) ,(length schema)))] ; no-check
    (for [i 1 (length schema)]
      (if
        (sym? (. schema i))
        (table.insert result
          `(if
            (fennel.sym? (. ,ast ,i))
            (= (?. ,ast ,i 1) ,(. schema i 1))
            false))

        (list? (. schema i))
        (table.insert result
          `(if
            (fennel.list? (. ,ast ,i))
            (list= (. ,ast ,i) ,(. schema i))
            false))

        (varg? (. schema i))
        (table.insert result true)

        (= nil (. schema i))
        (table.insert result
        `(if
          (fennel.sym? (. ,ast ,i))
          (= (?. ,ast ,i 1) :nil)
          false))

        ;; else
        (table.insert result `(= (. ,ast ,i) ,(. schema i)))))
    result))

{: defcheck
 : list=}
