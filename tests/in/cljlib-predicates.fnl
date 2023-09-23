(local x 1)

(= 0 x)
(= x 0)
(= 0 1 2)

(= nil x)
(= x nil)

(> x 0)
(< 0 x)

(< x 0)
(> 0 x)

(= 0 (% x 2))
(= (% x 2) 0)

(not= 0 (% x 2))
(not= (% x 2) 0)
(= 1 (% x 2))
(= (% x 2) 1)

(= :string (type x))
(= (type x) :string)

(= :boolean (type x))
(= (type x) :boolean)

(= true x)
(= x true)

(= false x)
(= x false)
