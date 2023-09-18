;; fennel-ls: macro-file
"Macros to define checks."

(local config ((. (require :config) :get)))

;(local list-checks [])
(local check-metadata {})

(λ defcheck [check-table code enabled? param docstring body]
  "Generic macro to define a check"
  `(let [default?# ,enabled?
         enabled?# (if (not= nil (. config.checks ,code)) (. config.checks ,code) default?#)]
    (tset ,check-metadata ,code {:docstring ,docstring :default? default?# :enabled? enabled?#})
    (when enabled?#
      (table.insert ,check-table
        (fn ,param "" ,body)))))

{: defcheck}
