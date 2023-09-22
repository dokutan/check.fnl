(import-macros {: defcheck} :macros)
(local {: check-warning} (require :utils))
(local config ((. (require :config) :get)))

(local string-checks [])
(macro string-check [code enabled? param docstring body]
  "Define a check for lists"
  `(defcheck string-checks :line #true ,code ,enabled? ,param ,docstring ,body))

(string-check :style-length true [context line number]
  "Checks if the line is to long"
  (let [max-line-length (or config.max-line-length 80)]
    (when (> (utf8.len line) max-line-length)
      (check-warning context number (.. "line length exceeds " max-line-length " columns")))))

(string-check :style-delimiters true [context line number]
  "Checks if closing delimiters appear on their own line"
  (when (string.match line "^[ \t]*[])}]+[ \t]*$")
    (check-warning context number "closing delimiters should not appear on their own line")))

string-checks
