#!/usr/bin/env fennel
(local fennel (require :fennel))

;;; miscellaneous functions
(local ast-checks [])
(local string-checks [])
(var current-lines [])

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

(fn warning [linenumber message]
  "Print a warning"
  (print (.. "\x1b[31m" linenumber ": " message "\x1b[0m\n" (. current-lines (tonumber linenumber)))))

;;; AST based checks
(ast-check true [ast]
  "Checks for deprecated forms"
  (let [deprecated {:require-macros "0.4.0"
                    :pick-args "0.10.0"
                    :global "1.1.0"}
        position (position->string ast)
        form (. ast 1)
        form (when (= :table (type form)) (. form 1))
        since (. deprecated form)]
    (when (not= nil since)
      (warning position (.. form " is deprecated since " since)))))

(ast-check true [ast]
  "Checks names for bad symbols"
  (let [forms {:local true
               :var true
               :global true
               :macro true
               :fn true}
        position (position->string ast)
        form (. ast 1)
        form (when (= :table (type form)) (. form 1))]
    (when (not= nil (. forms form))
      (let [name (?. ast 2 1)]
        (when (string.match (tostring name) "[A-Z_]")
          (warning position "don't use [A-Z_] in names"))))))

(ast-check true [ast]
  "Checks for if expressions that can be replaced with when"
  (let [position (position->string ast)
        form (. ast 1)
        form (when (= :table (type form)) (. form 1))]
    (when (and (= :if form) (< (length ast) 5))
      (let [else (. ast 4)
            else (if (= :table (type else)) (. else 1) else)]
        (when (or (= :nil else) (= nil else))
          (warning position "this if can be replaced with when"))))))

(ast-check true [ast]
  "Checks if functions and macros have docstrings"
  (let [position (position->string ast)
        form (. ast 1)
        form (when (= :table (type form)) (. form 1))]
    (when (or (= :fn form) (= :macro form))
      (when (not= :string (type (?. ast 4)))
        (warning position (.. form " " (tostring (?. ast 2 1)) " has no docstring"))))))

(ast-check true [ast]
  "Checks for useless do forms"
  (let [position (position->string ast)
        form (. ast 1)
        form (when (= :table (type form)) (. form 1))]
    (when (= :do form)
      (when (< (length ast) 3)
        (warning position "this do is useless")))))

(ast-check true [ast]
  "Checks for nested do forms"
  (let [form (. ast 1)
        form (when (= :table (type form)) (. form 1))]
    (when (= :do form)
      (each [_ v (pairs ast)]
        (when (= :table (type v))
          (when (not= nil (. (getmetatable v) "__fennelview"))
            (let [form (?. v 1 1)
                  position (position->string v)]
              (when (= :do form)
                (warning position "this nested do is useless")))))))))

(ast-check true [ast]
  "Checks for invalid let bindings"
  (let [position (position->string ast)
        form (. ast 1)
        form (when (= :table (type form)) (. form 1))]
    (when (= :let form)
      (let [bindings (?. ast 2)]
        (if
          (not= :table (type bindings))
          (warning position "let requires a table as the first argument")
          (not= 0 (% (length bindings) 2))
          (warning position "let requires an even number of bindings"))
        (when (< (length ast) 3)
          (warning position "let requires a body"))))))

(fn perform-ast-checks [ast]
  "Recursively performs checks on the AST"
  (each [_ check (ipairs ast-checks)]
    (check ast))
  (each [_ v (pairs ast)]
    (when (= :table (type v)) ; table?
      (when (not= nil (. (getmetatable v) "__fennelview")) ; nested ast?
        (perform-ast-checks v)))))

;;; string based checks
(string-check true [line number]
  "Checks if the line length exceeds 80 columns"
  (when (> (utf8.len line) 80)
    (warning number "line length exceeds 80 columns")))

(string-check true [line number]
  "Checks if comments start with the correct number of semicolons"
  (when (string.match line "^[ \t]*;[^;]+")
    (warning number "this comment should start with at least two semicolons"))
  (when (string.match line "[^ \t;]+[ \t]*;;")
    (warning number "this comment should start with one semicolon")))

(string-check true [line number]
  "Checks if closing delimiters appear on their own line"
  (when (string.match line "^[ \t]*[])}]+")
    (warning number "closing delimiters should not appear on their own line")))

(fn perform-string-checks []
  "Perfoms checks on each line in `file`"
  (each [number line (ipairs current-lines)]
    (each [_ check (ipairs string-checks)]
      (check line number))))

;;; main
(each [_ file (ipairs arg)]
  (print (.. "\x1b[32m" file "\x1b[0m"))
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
            (perform-ast-checks ast)
            (iterate)))))
    (iterate))
  (print))