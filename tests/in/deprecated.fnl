(global g 1)
(pick-args 2 +)
(faccumulate [result "" i 1 10 :until (= i 5)]
  (.. result i))
