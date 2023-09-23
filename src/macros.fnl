;; fennel-ls: macro-file
"Macros to define checks."

(λ defcheck [check-table check-type apply? name enabled? param docstring & body]
  "Generic macro to define a check"
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
       :fn (fn ,param ,docstring ,(unpack body))})))

(fn list= [ast schema]
  "Checks if `ast` matches `schema`."
  (let [result `(and (= (length ,ast) ,(length schema)))]
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
            (list= (. ,ast ,i) ,(. schema i)
            false)))

        (varg? (. schema i))
        (table.insert result true)

        (= nil (. schema i))
        (table.insert result
        `(if
          (fennel.sym? (. ,ast ,i))
          (= (?. ,ast ,i 1) :nil)
          false))

        ;else
        (table.insert result `(= (. ,ast ,i) ,(. schema i)))))
    result))

{: defcheck
 : list=}
