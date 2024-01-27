(local fennel (require :fennel))
(local config ((. (require :config) :get)))

;;; ansi escape codes to colorize the output
(local color
  (if (not= false config.color)
    {:red "\x1b[31m" :yellow "\x1b[33m" :blue "\x1b[34m" :default "\x1b[0m"}
    {:red "" :yellow "" :blue "" :default ""}))

;;; miscellaneous functions
(fn unpack* [tbl]
  "A wrapper around `table.unpack`/`unpack` for compatibility with luajit."
  (if table.unpack
    (values (table.unpack tbl))
    (values (_G.unpack tbl)))) ; no-check

(fn ??. [t k ...]
  "Type-safe table lookup, returns nil if `t` is not a table"
  (if (not= 0 (length [...]))
    (when (= :table (type t))
      (??. (. t k) (unpack* [...])))
    (when (= :table (type t))
      (. t k))))

(fn sym= [sym name]
  "Is `sym` a Fennel symbol having `name` ?"
  (and (fennel.sym? sym) (= name (??. sym 1))))

(fn print-table [tab depth]
  "Print table `tab`, intended for debugging"
  (let [depth (if depth depth 0)]
    (print (.. (string.rep "  " depth) "table: " (tostring tab)))
    (each [k v (pairs tab)]
      (if (not= :table (type v))
        (print (.. (string.rep "  " depth) k " " (tostring v)))
        (print-table v (+ 1 depth))))))

(fn position->string [node]
  "Returns the position of a AST node as a string"
  (if
    (not= nil (. node :line))
      (tostring (. node :line))
    (not= nil (. (getmetatable node) :line))
      (tostring (. (getmetatable node) :line))
    "?"))

(fn check-warning [context linenumber message]
  "Print a warning"
  (when (not (?. context :skip-current-lines (tostring linenumber)))
    (when (= (. context :return-value) 0) (tset context :return-value 1))
    (print (.. color.yellow
               linenumber ": "
               message
               color.default "\n"
               (?. context :current-lines (tonumber linenumber))))))

(fn check-error [context linenumber message]
  "Print an error"
  (when (not (?. context :skip-current-lines (tostring linenumber)))
    (tset context :return-value 2)
    (print (.. color.red
               linenumber ": "
               message
               color.default "\n"
               (?. context :current-lines (tonumber linenumber))))))

{: color
 : ??.
 : sym=
 : print-table
 : position->string
 : check-warning
 : check-error}
