(let [a 1] a)
(for [i 1 2])
(when true)

(faccumulate [r 0 i 1 9] (print r) (+ r i))
(fcollect [i 1 5] (print i) i)
(collect [k v {:a :b}] (print k v) k v)

(faccumulate [r 0 i 1 9] (+ r i))
(fcollect [i 1 5] i)
(collect [k v {:a :b}] k v)
