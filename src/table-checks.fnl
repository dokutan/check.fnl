(import-macros {: defcheck} :macros)
(local fennel (require :fennel))
(local {: position->string : check-warning} (require :utils))

(fn kv-table? [ast]
  "Is `ast` a kv-table?"
  (and
    (= :table (type ast))
    (not (fennel.list? ast))
    (not (fennel.sym? ast))
    (not (fennel.varg? ast))
    (not (fennel.comment? ast))
    (not (fennel.sequence? ast))))

(local table-checks [])
(macro table-check [code enabled? param docstring body]
  "Define a check for lists"
  `(defcheck table-checks
             :ast
             kv-table?
             ,code
             ,enabled?
             ,param
             ,docstring
             ,body))

(table-check :duplicate-keys true [context ast]
  "Checks for duplicate keys in tables"
  (let [position (position->string ast)
        keys {}]
    (each [_ k (ipairs (. (getmetatable ast) :keys))]
      (when (. keys k)
        (check-warning context
                       position
                       (.. "key " (tostring k) " occurs multiple times")))
      (tset keys k true))))

(table-check :table->sequence true [context ast]
  "Checks for tables that can be written as sequences"
  (let [position (position->string ast)
        keys {}]
    (each [_ k (ipairs (. (getmetatable ast) :keys))]
      (tset keys k true))
    (when (< 0 (length ast))
      ((fn iterate [i sequence?] ; no-check
        (if
          (<= i (length (. (getmetatable ast) :keys)))
            (iterate
              (+ 1 i)
              (and sequence? (. keys i)))
          (and sequence? (< (length (. (getmetatable ast) :keys)) i))
            (check-warning context
                           position
                           "this table can be written as a sequence"))
        ) 1 true))))

table-checks
