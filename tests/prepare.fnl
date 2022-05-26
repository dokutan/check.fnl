#!/usr/bin/env fennel

(each [in (: (io.popen "find ./in -name \"*.fnl\"") :lines)] ; for each file in ./in
  (let [out (string.gsub (string.gsub in ".fnl$" ".txt") "^./in/" "./out/")]
    (when (not (io.open out))
      (with-open [out-file (io.open out :w)]
        (print (.. "created " out))
        (let [out-contents (: (io.popen (.. "../check.fnl -c test-config.fnl " in)) :read "*a")]
          (out-file:write out-contents))))))