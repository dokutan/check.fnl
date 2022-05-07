#!/usr/bin/env fennel
(local fennel (require :fennel))

;;; miscellaneous functions
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

(fn warning [line number message]
  "Print a warning"
  (print (.. "\x1b[31m" number ": " message "\x1b[0m\n" line "\n")))

;;; AST based checks
(fn check-deprecated [ast]
  "Checks for deprecated forms"
  (let [deprecated {:require-macros "0.4.0"
                    :pick-args "0.10.0"
                    :global "1.1.0"}
        position (position->string ast)
        form (?. ast 1 1)
        since (. deprecated form)]
    (when (not= nil since)
      (warning (tostring ast) position (.. form " is deprecated since " since)))))

(fn check-names [ast]
  "Checks names for bad symbols"
  (let [forms {:local true
               :var true
               :global true
               :macro true
               :fn true}
        position (position->string ast)
        form (?. ast 1 1)]
    (when (not= nil (. forms form))
      (let [name (?. ast 2 1)]
        (when (string.match (tostring name) "[A-Z_]")
          (warning (tostring ast) position "don't use [A-Z_] in names"))))))

(fn ast-checks [ast]
  "Recursively performs checks on the AST"
  (doto ast
    (check-deprecated)
    (check-names))
  (each [k v (pairs ast)]
    (when (= :table (type v)) ; table?
      (when (not= nil (. (getmetatable v) "__fennelview")) ; nested ast?
        (ast-checks v)))))

;;; string based checks
(fn check-line-length [line number]
  "Checks if the line length exceeds 80 columns"
  (when (> (utf8.len line) 80)
    (warning line number "line length exceeds 80 columns")))

(fn check-comments [line number]
  "Checks if comments start with the correct number of semicolons"
  (when (string.match line "^[ \t]*;[^;]+")
    (warning line number "this comment should start with at least two semicolons"))
  (when (string.match line "[^ \t;]+[ \t]*;;")
    (warning line number "this comment should start with one semicolon")))

(fn check-closing [line number]
  "Checks if closing delimiters appear on their own line"
  (when (string.match line "^[ \t]*[])}]+")
    (warning line number "closing delimiters should not appear on their own line")))

(fn string-checks [file]
  "Perfoms checks on each line in `file`"
  (let [lines []]
    (each [line (file:lines)]
      (table.insert lines line))
    (each [number line (ipairs lines)]
      (check-line-length line number)
      (check-comments line number)
      (check-closing line number))))

;;; main
(each [_ file (ipairs arg)]
  (print (.. "\x1b[32m" file "\x1b[0m"))
  (let [file (io.open file)
        str (file:read "*a")
        parse (fennel.parser str)]
    (fn iterate []
      (let [(ok ast) (parse)]
        (when ok
          (do
            (ast-checks ast)
            (iterate)))))
    (iterate))
  (let [file (io.open file)]
    (string-checks file)))