(import-macros {: defcheck} :macros)
(local {: position->string : check-warning} (require :utils))
(local config ((. (require :config) :get)))

(local comment-checks [])
(macro comment-check [code enabled? param docstring body]
  "Define a check for lists"
  `(defcheck comment-checks ,code ,enabled? ,param ,docstring ,body))

(comment-check :style-comments true [context ast]
  "Checks if comments start with the correct number of semicolons"
  (let [linenumber (position->string ast)]
    (when (not= :? linenumber)
      (let [linenumber (tonumber linenumber)
            line (?. context :current-lines linenumber)
            comment-string (tostring ast)
            code-string (string.sub line 1 (- (length line) (length comment-string)))]

        (when (and (string.match code-string "^[ \t]*$")
                   (string.match comment-string "^;[^;]*$"))
          (check-warning
            context
            linenumber
            "this comment should start with at least two semicolons"))

        (when (and (string.match code-string "[^ \t;]+[ \t]*$")
                   (string.match comment-string "^;;"))
          (check-warning
            context
            linenumber
            "this comment should start with one semicolon"))))))

comment-checks
