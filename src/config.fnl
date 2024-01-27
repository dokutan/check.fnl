"Load configuration from a file."

(local fennel (require :fennel))

(var config
  {:color true
   :max-line-length nil
   :anonymous-docstring false
   :checks {}})

(λ get-config []
  "Return the current config."
  config)

(λ load-config [file]
  "Load config from `file`."
  (set config (fennel.dofile file))
  (when (= nil config.checks)
    (tset config :checks {})))

{:get get-config
 :load load-config}
