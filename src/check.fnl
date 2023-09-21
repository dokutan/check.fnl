#!/usr/bin/env fennel

(macro load-checks [checks module]
  "Load the checks from `module` into `checks`."
  `(each [k# v# (pairs (require ,module))]
    (tset ,checks k# v#)))

;;; parse commandline arguments
(var config-path nil)
(var show-checks? false)
(local files [])
(while (. arg 1)
  (if
    (= "-h" (. arg 1)) (do
            (print "usage: check.fnl [-s] [-c config] file ...")
            (os.exit 0))
    (= "-s" (. arg 1)) (do
            (set show-checks? true)
            (table.remove arg 1))
    (= "-c" (. arg 1)) (do
            (set config-path (. arg 2))
            (table.remove arg 1)
            (table.remove arg 1))
    (not= nil (. arg 1))
      (do
        (table.insert files (. arg 1))
        (table.remove arg 1))))

;;; load config, this needs to be done before other modules are required
(when config-path
  ((. (require :config) :load) config-path))
(local config ((. (require :config) :get)))

;;; require
(local fennel (require :fennel))
(local {: position->string : color : check-error} (require :utils))

;;; local variables
(var current-lines [])
(var skip-current-lines {})
(var current-symbols {})
(var return-value 0)

;;; require checks
(local checks {})
(local check-names [])
(load-checks checks :list-checks)
(load-checks checks :sym-checks)
(load-checks checks :table-checks)
(load-checks checks :comment-checks)
(load-checks checks :string-checks)

;; generate a sorted list of check names to enable stable iteration
(each [name (pairs checks)]
  (table.insert check-names name))
(table.sort check-names)

;;; show checks?
(when show-checks?
  (each [_ name (ipairs check-names)]
    (let [metadata (. checks name)
          pad1 (string.rep " " (- 20 (length name)))
          enabled? (.. (if metadata.enabled? "true" "false") "(" (if metadata.default? "true" "false") ")")
          pad2 (string.rep " " (- 13 (length enabled?)))]
      (print (.. name pad1 enabled? pad2 (or metadata.docstring "")))))
  (os.exit 0))

;;; main
(fn perform-ast-checks [context ast root?]
  "Recursively performs checks on the AST"
  (each [_ name (ipairs check-names)]
    (let [check (. checks name)]
      (when (and (= :ast check.type) (check.apply? ast))
        (check.fn context ast root?))))
  (when (= :table (type ast))
    (each [_ v (pairs ast)]
      (when (= :table (type v)) ; nested ast or table?
        (perform-ast-checks context v false)))))

(fn perform-string-checks []
  "Perfoms checks on each line in `file`"
  (each [number line (ipairs current-lines)]
    (each [_ check (pairs checks)]
      (when (= :line check.type)
        (check.fn {: current-lines : skip-current-lines : current-symbols} line number)))))

(fn parse-directives [ast]
  "Check if `ast` contains comments to disable checks"
  (if (fennel.comment? ast)
    (let [linenumber (position->string ast)]
      (when (and (not= :? linenumber)
                 (string.match (tostring ast) "no%-check"))
        (tset skip-current-lines (->> linenumber tonumber tostring) true)))
    (when (= :table (type ast))
      (each [_ v (pairs ast)]
        (when (= :table (type v)) ; nested ast or table?
          (parse-directives v))))))

(each [_ file (ipairs files)]
  (print (.. color.blue file color.default))
  (set current-symbols {})
  (set current-lines [])
  (set skip-current-lines {})
  (var skip-file false)

  (with-open [f (io.open file)]
    ;; read all lines from file as a table
    (each [line (f:lines)]
      (table.insert current-lines line))

    ;; read whole file as a string
    (f:seek :set 0)
    (let [current-file (f:read "*a")]

      ;; parse directives to skip checks
      (let [parse (fennel.parser current-file nil {:comments true})]
        (fn iterate []
          (let [(pcall-ok parse-ok ast) (pcall parse)]
            (if
              (and pcall-ok parse-ok)
              (do
                (parse-directives ast)
                (iterate))

              (not pcall-ok)
              (do
                (set skip-file true)
                (check-error 1 parse-ok)))))
        (iterate))

      (when (not skip-file)
        ;; perform string based checks
        (perform-string-checks)

        ;; perform AST-based checks
        (let [parse (fennel.parser current-file nil {:comments true})
              context {: current-lines : skip-current-lines : current-symbols}]
          (fn iterate []
            (let [(pcall-ok parse-ok ast) (pcall parse)]
              (when (and pcall-ok parse-ok)
                (perform-ast-checks context ast true)
                (iterate))))
          (iterate)))))

  (print))

(os.exit return-value)
