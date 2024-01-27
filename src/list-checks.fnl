(import-macros {: defcheck : list=} :macros)
(local fennel (require :fennel))
(local {: ??. : position->string : check-warning : check-error : sym=} (require :utils))

(local config ((. (require :config) :get)))

(local list-checks [])
(macro list-check [code enabled? param docstring body]
  "Define a check for lists"
  `(defcheck list-checks :ast fennel.list? ,code ,enabled? ,param ,docstring ,body))

(list-check :deprecated true [context ast]
  "Checks for deprecated forms"
  (let [deprecated {:require-macros "0.4.0"
                    :pick-args "0.10.0"
                    :global "1.1.0"}
        position (position->string ast)
        form (??. ast 1 1)
        since (. deprecated form)]
    (when (and (fennel.sym? (. ast 1)) (not= nil since))
      (check-warning context position (.. form " is deprecated since " since)))))

(list-check :deprecated-clause true [context ast]
  "Checks for the use of :until/:into or instead of &until/&into"
  (let [forms {:for true
               :each true
               :icollect true
               :collect true
               :fcollect true
               :accumulate true
               :faccumulate true}
        position (position->string ast)
        form (??. ast 1 1)]
    (when (. forms form)
      (let [bindings (??. ast 2)]
        (when (fennel.table? bindings)
          (let [clauses
                (icollect [_ v (pairs bindings)]
                  (if (or (= :until v) (= :into v))
                    v nil))]
            (each [_ v (ipairs clauses)]
              (if
                (= v :until)
                (check-warning context position (.. ":until is deprecated in " form ", use &until"))
                (= v :into)
                (check-warning context position (.. ":into is deprecated in " form ", use &into"))))))))))


(list-check :if->when true [context ast]
  "Checks for if expressions that can be replaced with when"
  (let [position (position->string ast)
        form (. ast 1)]
    (when (and (sym= form :if) (< (length ast) 5))
      (let [else (??. ast 4)]
        (when (or (sym= else :nil) (= nil else))
          (check-warning context position "if the body causes side-effects, replace this if with when"))))))

(list-check :docstring true [context ast]
  "Checks if functions and macros have docstrings"
  (let [position (position->string ast)
        form (. ast 1)
        has-docstring?
        (fn [ast pos]
          (or (= :string (type (?. ast pos)))
              (and (= :table (type (?. ast pos)))
                   (= :string (type (??. (?. ast pos) :fnl/docstring))))))]

    (when (or (sym= form :fn) (sym= form :macro) (sym= form :lambda) (sym= form :λ))
      (if (fennel.sequence? (?. ast 2))
        (when (and config.anonymous-docstring
                   (or (<= (length ast) 3) (not (has-docstring? ast 3))))
          (check-warning context position (.. "anonymous " (. form 1) " has no docstring")))
        (when (or (<= (length ast) 4) (not (has-docstring? ast 4)))
          (check-warning context position (.. (. form 1) " " (tostring (?. ast 2 1)) " has no docstring")))))))

(list-check :useless-do true [context ast]
  "Checks for useless do forms"
  (let [forms {:let 2
               :fn 2
               :lambda 2
               :λ 2
               :do 2
               :when 3}
        form (??. ast 1 1)]
    (when (. forms form)
      (each [index node (pairs ast)]
        (when (fennel.list? node)
          (let [form2 (. node 1)
                position (position->string node)]
            (when (and
                    (sym= form2 :do)
                    (<= (. forms form) index))
              (check-warning context position (.. "do is useless inside of " form)))))))))

(list-check :syntax-let true [context ast]
  "Checks for invalid let bindings"
  (let [position (position->string ast)
        form (. ast 1)]
    (when (sym= form :let)
      (let [bindings (??. ast 2)]
        (when (and (fennel.table? bindings) (= 0 (length bindings)))
          (check-warning context position "let has no bindings"))
        (when (and (fennel.table? bindings) (not= 0 (% (length bindings) 2)))
          (check-error context position "let requires an even number of bindings"))))))

(list-check :syntax-no-bindings true [context ast]
  "Checks for missing binding tables"
  (let [forms {:let true
               :for true
               :each true
               :icollect true
               :collect true
               :fcollect true
               :accumulate true
               :faccumulate true}
        position (position->string ast)
        form (??. ast 1 1)]
    (when (. forms form)
      (let [bindings (??. ast 2)]
        (when (not (fennel.sequence? bindings))
          (check-error context position (.. form " requires a binding table as the first argument")))))))

(list-check :syntax-body true [context ast]
  "Checks for missing or wrong body expressions"
  (let [forms {:let true
               :for true
               :icollect 1
               :collect 2
               :fcollect 1
               :accumulate 1
               :faccumulate 1
               :when true}
        position (position->string ast)
        form (??. ast 1 1)]
    (when (and (. forms form) (< (length ast) 3))
      (check-error context position (.. form " requires a body")))
    (when (and (= 1 (. forms form)) (> (length ast) 3))
      (check-error context position (.. form " requires exactly one body expression")))
    (when (and (= 2 (. forms form)) (> (length ast) 4))
      (check-error context position (.. form " requires exactly one or two body expressions")))))

(list-check :syntax-if true [context ast]
  "Checks for invalid uses of if"
  (let [position (position->string ast)
        form (. ast 1)]
    (when (sym= form :if)
      (when (< (length ast) 3)
        (check-error context position "if requires a condition and a body")))))

(list-check :syntax-for true [context ast]
  "Checks for syntax errors in the for binding table"
  (let [position (position->string ast)
        form (. ast 1)
        bindings (??. ast 2)]
    (when (and
            (sym= form :for)
            (fennel.sequence? bindings)
            (< (length bindings) 3))
      (check-error context position "for requires a binding table with a symbol, start and stop points"))))

(list-check :for->each true [context ast]
  "Checks for uses of for that should be replaced with each"
  (let [position (position->string ast)
        form (. ast 1)
        bindings (??. ast 2)]
    (when (and
            (sym= form :for)
            (fennel.sequence? bindings))
      (each [_ i (pairs bindings)]
        (when (and
                (fennel.list? i)
                (or (sym= (. i 1) :pairs)
                    (sym= (. i 1) :ipairs)))
          (check-warning context position "use each instead of for for general iteration"))))))

(list-check :useless-not true [context ast]
  "Checks for uses of not that can be replaced"
  (let [position (position->string ast)
        form (. ast 1)]
    (when (sym= form :not)
      (let [form (??. ast 2 1)]
        (if
          (sym= form :not)
          (check-warning context position "(not (not ...)) is useless")
          (sym= form :not=)
          (check-warning context position "replace (not (not= ...)) with (= ...)")
          (sym= form "~=")
          (check-warning context position "replace (not (~= ...)) with (= ...)")
          (sym= form :=)
          (check-warning context position "replace (not (= ...)) with (not= ...)")
          (sym= form :<)
          (check-warning context position "replace (not (< ...)) with (>= ...)")
          (sym= form :<=)
          (check-warning context position "replace (not (<= ...)) with (> ...)")
          (sym= form :>)
          (check-warning context position "replace (not (> ...)) with (<= ...)")
          (sym= form :>=)
          (check-warning context position "replace (not (>= ...)) with (< ...)"))))))

(list-check :identifier false [context ast]
  "Checks for lists that don't begin with an identifier"
  (let [position (position->string ast)
        form (??. ast 1)]
    (when (and (fennel.list? ast) (not (fennel.sym? form)))
      (check-warning context position "this list doesn't begin with an identifier"))))

(list-check :local->let true [context ast root?]
  "Checks for locals that can be replaced with let"
  (let [position (position->string ast)
        form (??. ast 1)]
    (when (and (not root?) (sym= form :local))
      (check-warning context position "this local can be replaced with let"))))

(list-check :syntax-relational true [context ast]
  "Checks for relational operators that are missing an operand"
  (let [forms {:= true "~=" true :not= true :< true :<= true :> true :>= true}
        position (position->string ast)
        form (??. ast 1 1)]
    (when (and
            (fennel.sym? (. ast 1))
            (not= nil (. forms form))
            (< (length ast) 3))
      (check-error context position (.. form " requires at least two arguments")))))

(list-check :useless-forms true [context ast]
  "Checks for forms that are useless with one argument"
  (let [forms {:+ true :% true :* true :. true :.. true :// true :?. true
               :^ true :or true :and true :math.min true :math.max true
               :do true :doto true :-> true :->> true :-?> true :-?>> true}
        position (position->string ast)
        form (??. ast 1 1)]
    (when (and
            (fennel.sym? (. ast 1))
            (not= nil (. forms form))
            (< (length ast) 3))
      (check-warning context position (.. form " is useless with a single argument")))))

(list-check :style-alternatives true [context ast]
  "Checks for forms that have multiple names"
  (let [position (position->string ast)
        form (. ast 1)]
    (if
      (sym= form :#)
      (check-warning context position "# can be replaced with length")
      (sym= form "~=")
      (check-warning context position "~= can be replaced with not="))))

(list-check :redefine true [context ast root?]
  "Checks for redefined symbols"
  (when root?
    (fn check-symbol [symbol position form]
      "Checks if `symbol` has been previously defined, otherwise store it"
      (if
        (and symbol (. context :current-symbols symbol))
          (check-warning context
            position
            (..
              "this " form " redefines "
              (. context :current-symbols symbol :form) " " symbol
              " (line " (. context :current-symbols symbol :position) ")"))
        symbol
          (tset
            context.current-symbols
            symbol
            {:position position :form form})))
      (let [form (. ast 1)
            position (position->string ast)]
        (if
          (or (sym= form :global) (sym= form :local)
              (sym= form :var) (sym= form :macro))
            (check-symbol (. ast 2 1) position (. form 1))
          (or (sym= form :fn) (sym= form :λ) (sym= form :lambda))
            (if (fennel.sym? (. ast 2))
              (check-symbol (. ast 2 1) position (. form 1)))
          (sym= form :macros)
            (each [k (pairs (. ast 2))]
              (check-symbol k position (. form 1)))))))

(list-check :shadow true [context ast root?]
  "Checks for shadowed symbols"
  (do
    (fn check-symbol [symbol position form root?]
      "Checks if `symbol` has been previously defined"
      (if
        (and symbol (. context :current-symbols symbol) (not root?))
          (check-warning context
            position
            (..
              "this " form " shadows "
              (. context :current-symbols symbol :form) " " symbol
              " (line " (. context :current-symbols symbol :position) ")"))))
      (let [form (. ast 1)
            position (position->string ast)]
        (if
          (or (sym= form :global) (sym= form :local)
              (sym= form :var) (sym= form :macro))
            (check-symbol (. ast 2 1) position (. form 1) root?)
          (or (sym= form :fn) (sym= form :λ) (sym= form :lambda))
            (if (fennel.sym? (. ast 2))
              (check-symbol (. ast 2 1) position (. form 1) root?))
          (and (= :table (type (. ast 2))) (sym= form :macros))
            (each [k (pairs (. ast 2))]
              (check-symbol k position (. form 1) root?))
          (and (= :table (type (. ast 2))) (sym= form :let))
            (each [k v (ipairs (. ast 2))]
              (when (= 1 (% k 2))
                (check-symbol (. v 1) position (. form 1) false)))))))

(list-check :style-bad-forms true [context ast]
  "Checks for forms that should be avoided"
  (let [forms {:> true :>= true :hashfn true :eval-compiler true :lua true}
        position (position->string ast)
        form (??. ast 1 1)]
    (when (. forms form)
      (check-warning context position (.. "avoid using " form)))))

(list-check :cljlib/predicates false [context ast]
  "Checks for comparisons that can be repleced with cljlib predicates"
  (let [position (position->string ast)]
    (if
      (or (list= ast (= nil ...)) (list= ast (= ... nil)))
      (check-warning context position "use cljlib.nil? instead of (= nil ...)")

      (or (list= ast (= 0 ...)) (list= ast (= ... 0)))
      (check-warning context position "use cljlib.zero? instead of (= 0 ...)")

      (list= ast (> ... 0))
      (check-warning context position "use cljlib.pos? instead of (> ... 0)")

      (list= ast (< 0 ...))
      (check-warning context position "use cljlib.pos? instead of (< 0 ...)")

      (list= ast (< ... 0))
      (check-warning context position "use cljlib.neg? instead of (< ... 0)")

      (list= ast (> 0 ...))
      (check-warning context position "use cljlib.neg? instead of (> 0 ...)")

      (or (list= ast (= (% ... 2) 0)) (list= ast (= 0 (% ... 2))))
      (check-warning context position "use cljlib.even? instead of (= 0 (% ... 2))")

      (or (list= ast (not= (% ... 2) 0)) (list= ast (not= 0 (% ... 2))))
      (check-warning context position "use cljlib.odd? instead of (not= 0 (% ... 2))")

      (or (list= ast (= (% ... 2) 1)) (list= ast (= 1 (% ... 2))))
      (check-warning context position "use cljlib.odd? instead of (= 1 (% ... 2))")

      (or (list= ast (= (type ...) :string)) (list= ast (= :string (type ...))))
      (check-warning context position "use cljlib.string? instead of (= :string (type ...))")

      (or (list= ast (= (type ...) :boolean)) (list= ast (= :boolean (type ...))))
      (check-warning context position "use cljlib.boolean? instead of (= :boolean (type ...))")

      (or (list= ast (= true ...)) (list= ast (= ... true)))
      (check-warning context position "use cljlib.true? instead of (= true ...)")

      (or (list= ast (= false ...)) (list= ast (= ... false)))
      (check-warning context position "use cljlib.false? instead of (= false ...)"))))

list-checks
