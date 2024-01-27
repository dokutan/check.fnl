(import-macros {: defcheck} :macros)
(local {: check-warning} (require :utils))
(local config ((. (require :config) :get)))

(fn len [str]
  "A wrapper aroiund `utf8.len`/`length` for compatibility with luajit."
  (if (?. _G :utf8 :len) ; no-check
    (_G.utf8.len str)    ; no-check
    (length str)))

(local string-checks [])
(macro string-check [code enabled? param docstring body]
  "Define a check for lists"
  `(defcheck string-checks
             :line
             #true ; no-check
             ,code
             ,enabled?
             ,param
             ,docstring
             ,body))

(string-check :style-length true [context line number]
  "Checks if the line is to long"
  (let [max-line-length (or config.max-line-length 80)]
    (when (< max-line-length (len line))
      (check-warning context
                     number
                     (.. "line length exceeds " max-line-length " columns")))))

(string-check :style-delimiters true [context line number]
  "Checks if closing delimiters appear on their own line"
  (when (string.match line "^[ \t]*[])}]+[ \t]*$")
    (check-warning context
                   number
                   "closing delimiters should not appear on their own line")))

string-checks
