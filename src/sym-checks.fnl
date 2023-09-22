(import-macros {: defcheck} :macros)
(local fennel (require :fennel))
(local {: position->string : check-warning} (require :utils))

(local sym-checks [])
(macro sym-check [code enabled? param docstring body]
  "Define a check for lists"
  `(defcheck sym-checks :ast fennel.sym? ,code ,enabled? ,param ,docstring ,body))

(sym-check :symbols true [context ast]
  "Checks names for bad symbols"
  (let [position (position->string ast)
        name (?. ast 1)]
    (when (and
            (= :string (type name))
            (string.match (string.sub name 2) "[A-Z_]+"))
      (check-warning context position "don't use [A-Z_] in names"))))

sym-checks
