(import-macros {: defcheck} :src.macros)
(local {: position->string : check-warning} (require :src.utils))
(local config ((. (require :src.config) :get)))

(local table-checks [])
(macro table-check [code enabled? param docstring body]
  "Define a check for lists"
  `(defcheck table-checks ,code ,enabled? ,param ,docstring ,body))

(table-check :duplicate-keys true [context ast]
  "Checks for duplicate keys in tables"
  (let [position (position->string ast)
        keys {}]
    (each [_ k (ipairs (. (getmetatable ast) :keys))]
      (when (. keys k)
        (check-warning context position (.. "key " (tostring k) " occurs multiple times")))
      (tset keys k true))))

(table-check :table->sequence true [context ast]
  "Checks for tables that can be written as sequences"
  (let [position (position->string ast)
        keys {}]
    (each [_ k (ipairs (. (getmetatable ast) :keys))]
      (tset keys k true))
    (when (> (length ast) 0)
      ((fn iterate [i sequence?]
        (if
          (<= i (length (. (getmetatable ast) :keys)))
            (iterate
              (+ 1 i)
              (and sequence? (. keys i)))
          (and sequence? (> i (length (. (getmetatable ast) :keys))))
            (check-warning context position "this table can be written as a sequence"))
        ) 1 true))))

table-checks
