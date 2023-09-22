#!/usr/bin/env fennel
(local fennel (require :fennel))

(macro load-checks [checks module]
  "Load the checks from `module` into `checks`."
  `(each [k# v# (pairs (require ,module))]
    (tset ,checks k# v#)))

(fn parse-arg []
  "Parse the commandline arguements. Returns three values:
   - files
   - config-path
   - show-checks?"
  (var config-path nil)
  (var show-checks? false)
  (local files [])

  (while (. arg 1)
    (if
      (= "-h" (. arg 1))
      (do
        (print "usage: check.fnl [-s] [-c config] file ...")
        (os.exit 0))

      (= "-s" (. arg 1))
      (do
        (set show-checks? true)
        (table.remove arg 1))

      (= "-c" (. arg 1))
      (do
        (set config-path (. arg 2))
        (table.remove arg 1)
        (table.remove arg 1))

      (not= nil (. arg 1))
      (do
        (table.insert files (. arg 1))
        (table.remove arg 1))))

  (values files config-path show-checks?))

(fn show-checks [check-names checks]
  "Print a list of all checks."
  (each [_ name (ipairs check-names)]
    (let [metadata (. checks name)
          pad1 (string.rep " " (- 20 (length name)))
          enabled? (.. (if metadata.enabled? "true" "false") "(" (if metadata.default? "true" "false") ")")
          pad2 (string.rep " " (- 13 (length enabled?)))]
      (print (.. name pad1 enabled? pad2 (or metadata.docstring ""))))))

(fn perform-ast-checks [check-names checks context ast root?]
  "Recursively performs checks on the AST"
  (each [_ name (ipairs check-names)]
    (let [check (. checks name)]
      (when (and (= :ast check.type) (check.apply? ast))
        (check.fn context ast root?))))
  (when (= :table (type ast))
    (each [_ v (pairs ast)]
      (when (= :table (type v)) ; nested ast or table?
        (perform-ast-checks check-names checks context v false)))))

(fn perform-string-checks [context checks]
  "Perfoms checks on each line in `file`"
  (each [number line (ipairs context.current-lines)]
    (each [_ check (pairs checks)]
      (when (= :line check.type)
        (check.fn context line number)))))

(fn parse-directives [context ast]
  "Check if `ast` contains comments to disable checks"
  (local {: position->string} (require :utils))
  (if (fennel.comment? ast)
    (let [linenumber (position->string ast)]
      (when (and (not= :? linenumber)
                 (string.match (tostring ast) "no%-check"))
        (tset context.skip-current-lines (->> linenumber tonumber tostring) true)))
    (when (= :table (type ast))
      (each [_ v (pairs ast)]
        (when (= :table (type v)) ; nested ast or table?
          (parse-directives context v))))))

(fn check-file [path check-names checks]
  "Read file at `path`, and perform checks."
  (local {: color} (require :utils))
  (local context {:current-lines []
                  :current-file ""
                  :skip-current-lines {}
                  :current-symbols {}
                  :return-value 0})

  (print (.. color.blue path color.default))
  (with-open [f (io.open path)]

    ;; read all lines from file as a table
    (each [line (f:lines)]
      (table.insert context.current-lines line))

    ;; read whole file as a string
    (f:seek :set 0)
    (tset context :current-file (f:read "*a")))

  ;; parse directives
  (let [parse (fennel.parser context.current-file nil {:comments true})]
    (fn iterate []
      (let [(pcall-ok parse-ok ast) (pcall parse)]
        (when (and pcall-ok parse-ok)
          (parse-directives context ast)
          (iterate))))
    (iterate))

  ;; perform string based checks
  (perform-string-checks context checks)

  ;; perform AST-based checks
  (let [parse (fennel.parser context.current-file nil {:comments true})]
    (fn iterate []
      (let [(pcall-ok parse-ok ast) (pcall parse)]
        (when (and pcall-ok parse-ok)
          (perform-ast-checks check-names checks context ast true)
          (iterate))))
    (iterate))

  context.return-value)

(fn main []
  "Main function."
  (let [(files config-path show-checks?) (parse-arg)]

    ;; load config, this needs to be done before other modules are required
    (when config-path
      ((. (require :config) :load) config-path))

    ;; require
    (local {: position->string : color : check-error} (require :utils))

    ;; local variables
    (var return-value 0)

    ;; require checks
    (local checks {})
    (load-checks checks :list-checks)
    (load-checks checks :sym-checks)
    (load-checks checks :table-checks)
    (load-checks checks :comment-checks)
    (load-checks checks :string-checks)

    ;; generate a sorted list of check names to enable stable iteration
    (local check-names [])
    (each [name (pairs checks)]
      (table.insert check-names name))
    (table.sort check-names)

    ;; show checks
    (when show-checks?
      (show-checks check-names checks)
      (os.exit 0))

    ;; perform checks for each file
    (each [_ file (ipairs files)]
      (set return-value (math.max return-value (check-file file check-names checks)))
      (print))

    (os.exit return-value)))

;;; main
(main)
