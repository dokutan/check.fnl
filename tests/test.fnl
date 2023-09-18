#!/usr/bin/env fennel

(local color
  {:red "\x1b[31m" :green "\x1b[32m" :default "\x1b[0m"})

(var return-value 0)
(var tests 0)
(var tests-ok 0)

(each [in (: (io.popen "find ./in -name \"*.fnl\"") :lines)] ; for each file in ./in
  (let [out (string.gsub (string.gsub in ".fnl$" ".txt") "^./in/" "./out/")
        out-file (io.open out)]
    (set tests (+ 1 tests))
    (if out-file
      (let [expected (: out-file :read "*a")
            results (: (io.popen (.. "../check.fnl -c test-config.fnl " in)) :read "*a")]
        (if (= expected results)
          (do
            (set tests-ok (+ 1 tests-ok))
            (print (.. color.green "check.fnl '" in "' matches '" out "'" color.default)))
          (do
            (set return-value 1)
            (print (.. color.red "check.fnl '" in "' does not match '" out "'" color.default)))))
      (print (.. color.red "could not open " out color.default)))))

(print)
(print (.. tests-ok "/" tests " tests passed"))
(os.exit return-value)
