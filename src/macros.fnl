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

{: defcheck}
