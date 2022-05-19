#!/usr/bin/env fennel
(local fennel (require :fennel))

;;; miscellaneous functions
(local ast-checks [])
(local string-checks [])
(var current-lines [])
(var return-value 0)
(var config-path nil)
(local files [])

((fn parse-arg [arg]
  "Parse commandline arguments"
  (match (. arg 1)
    "-h" (do
           (print "usage: check.fnl [-c config] file ...")
           (os.exit 0))
    "-c" (do
           (set config-path (. arg 2))
           (table.remove arg 1)
           (table.remove arg 1)))
  (if (not= nil (. arg 1))
    (do
      (table.insert files (. arg 1))
      (table.remove arg 1)
      (parse-arg arg))))
  arg)

(local config
  (if (not= nil config-path)
    (fennel.dofile config-path)
    {}))

(local color
  (if (= false (. config :color))
    {:red "" :yellow "" :blue "" :default ""}
    {:red "\x1b[31m" :yellow "\x1b[33m" :blue "\x1b[34m" :default "\x1b[0m"}))

(macro ast-check [enabled? param ?docstring body]
  "Define an AST based check"
  `(when ,enabled?
    (table.insert ast-checks
      (fn ,param ,?docstring ,body))))

(macro string-check [enabled? param ?docstring body]
  "Define a string based check"
  `(when ,enabled?
    (table.insert string-checks
      (fn ,param ,?docstring ,body))))

(fn ??. [t k ...]
  "Type-safe table lookup, returns nil if `t` is not a table"
  (if (not= 0 (length [...]))
    (when (= :table (type t))
      (??. (. t k) (table.unpack [...])))
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

(fn position->string [pos]
  "Returns the position of a keyword from the AST as a string"
  (tostring (. pos :line)))

(fn check-warning [linenumber message]
  "Print a warning"
  (when (= return-value 0) (set return-value 1))
  (print (.. color.yellow linenumber ": " message color.default "\n" (. current-lines (tonumber linenumber)))))

(fn check-error [linenumber message]
  "Print an error"
  (set return-value 2)
  (print (.. color.red linenumber ": " message color.default "\n" (. current-lines (tonumber linenumber)))))


;;; AST based checks
(ast-check true [ast]
  "Checks for deprecated forms"
  (let [deprecated {:require-macros "0.4.0"
                    :pick-args "0.10.0"
                    :global "1.1.0"}
        position (position->string ast)
        form (??. ast 1 1)
        since (. deprecated form)]
    (when (and (fennel.sym? (. ast 1)) (not= nil since))
      (check-warning position (.. form " is deprecated since " since)))))

(ast-check true [ast]
  "Checks names for bad symbols"
  (let [forms {:local true
               :var true
               :global true
               :macro true
               :fn true
               :lambda true
               :λ true}
        position (position->string ast)
        form (??. ast 1 1)]
    (when (and (fennel.sym? (. ast 1)) (not= nil (. forms form)))
      (let [name (?. ast 2 1)]
        (when (and (= :string (type name)) (string.match (tostring name) "[A-Z_]"))
          (check-warning position "don't use [A-Z_] in names"))))))

(ast-check true [ast]
  "Checks for if expressions that can be replaced with when"
  (let [position (position->string ast)
        form (. ast 1)]
    (when (and (sym= form :if) (< (length ast) 5))
      (let [else (??. ast 4)]
        (when (or (sym= else :nil) (= nil else))
          (check-warning position "if the body causes side-effects, replace this if with when"))))))

(ast-check true [ast]
  "Checks if functions and macros have docstrings"
  (let [position (position->string ast)
        form (. ast 1)]
    (when (or (sym= form :fn) (sym= form :macro) (sym= form :lambda) (sym= form :λ))
      (if (fennel.sequence? (?. ast 2))
        (when (or (<= (length ast) 3) (not= :string (type (?. ast 3))))
          (check-warning position (.. "anonymous " (. form 1) " has no docstring")))
        (when (or (<= (length ast) 4) (not= :string (type (?. ast 4))))
          (check-warning position (.. (. form 1) " " (tostring (?. ast 2 1)) " has no docstring")))))))

(ast-check true [ast]
  "Checks for useless do forms"
  (let [position (position->string ast)
        form (. ast 1)]
    (when (sym= form :do)
      (when (< (length ast) 3)
        (check-warning position "this do is useless")))))

(ast-check true [ast]
  "Checks for nested do forms"
  (let [form (. ast 1)]
    (when (sym= form :do)
      (each [_ v (pairs ast)]
        (when (fennel.list? v)
          (let [form (. v 1)
                position (position->string v)]
            (when (sym= form :do)
              (check-warning position "this nested do is useless"))))))))

(ast-check true [ast]
  "Checks for invalid let bindings"
  (let [position (position->string ast)
        form (. ast 1)]
    (when (sym= form :let)
      (let [bindings (??. ast 2)]
        (if
          (or (not (fennel.sequence? bindings)) (fennel.sym? bindings))
          (check-error position "let requires a table as the first argument")
          (not= 0 (% (length bindings) 2))
          (check-error position "let requires an even number of bindings"))
        (when (< (length ast) 3)
          (check-error position "let requires a body"))))))

(ast-check true [ast]
  "Checks for invalid uses of when"
  (let [position (position->string ast)
        form (. ast 1)]
    (when (sym= form :when)
      (when (< (length ast) 3)
        (check-error position "when requires a body")))))

(ast-check true [ast]
  "Checks for invalid uses of if"
  (let [position (position->string ast)
        form (. ast 1)]
    (when (sym= form :if)
      (when (< (length ast) 3)
        (check-error position "if requires a condition and a body")))))

(ast-check true [ast]
  "Checks for uses of not that can be replaced"
  (let [position (position->string ast)
        form (. ast 1)]
    (when (sym= form :not)
      (let [form (??. ast 2 1)]
        (if
          (sym= form :not)
          (check-warning position "(not (not ...)) is useless")
          (sym= form :not=)
          (check-warning position "replace (not (not= ...)) with (= ...)")
          (sym= form "~=")
          (check-warning position "replace (not (~= ...)) with (= ...)")
          (sym= form :=)
          (check-warning position "replace (not (= ...)) with (not= ...)")
          (sym= form :<)
          (check-warning position "replace (not (< ...)) with (>= ...)")
          (sym= form :<=)
          (check-warning position "replace (not (<= ...)) with (> ...)")
          (sym= form :>)
          (check-warning position "replace (not (> ...)) with (<= ...)")
          (sym= form :>=)
          (check-warning position "replace (not (>= ...)) with (< ...)"))))))

(ast-check true [ast]
  "Checks for lists that don't begin with an identifier"
  (let [position (position->string ast)
        form (??. ast 1)]
    (when (and (fennel.list? ast) (not (fennel.sym? form)))
      (check-warning position "this list doesn't begin with an identifier"))))

(ast-check true [ast root?]
  "Checks for locals that can be replaced with let"
  (let [position (position->string ast)
        form (??. ast 1)]
    (when (and (not root?) (sym= form :local))
      (check-warning position "this local can be replaced with let"))))

(fn perform-ast-checks [ast root?]
  "Recursively performs checks on the AST"
  (each [_ check (ipairs ast-checks)]
    (check ast root?))
  (when (fennel.list? ast)
    (each [_ v (pairs ast)]
      (when (fennel.list? v) ; nested ast?
        (perform-ast-checks v false)))))

;;; string based checks
(string-check true [line number]
  "Checks if the line length exceeds 80 columns"
  (when (> (utf8.len line) 80)
    (check-warning number "line length exceeds 80 columns")))

(string-check true [line number]
  "Checks if comments start with the correct number of semicolons"
  (when (string.match line "^[ \t]*;[^;]+")
    (check-warning number "this comment should start with at least two semicolons"))
  (when (string.match line "[^ \t;]+[ \t]*;;")
    (check-warning number "this comment should start with one semicolon")))

(string-check true [line number]
  "Checks if closing delimiters appear on their own line"
  (when (string.match line "^[ \t]*[])}]+")
    (check-warning number "closing delimiters should not appear on their own line")))

(fn perform-string-checks []
  "Perfoms checks on each line in `file`"
  (each [number line (ipairs current-lines)]
    (each [_ check (ipairs string-checks)]
      (check line number))))

;;; main
(each [_ file (ipairs files)]
  (print (.. color.blue file color.default))
  ;; read all lines from file
  (set current-lines [])
  (let [file (io.open file)]
    (each [line (file:lines)]
      (table.insert current-lines line)))
  ;; perform string based checks
  (perform-string-checks)
  ;; perform AST based checks
  (let [file (io.open file)
        str (file:read "*a")
        parse (fennel.parser str)]
    (fn iterate []
      (let [(ok ast) (parse)]
        (when ok
          (do
            (perform-ast-checks ast true)
            (iterate)))))
    (iterate))
  (print))
(os.exit return-value)
