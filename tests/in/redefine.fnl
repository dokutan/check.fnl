(local a 1)
(local a 2)

(var b 1)
(fn b [] "" 2)

(fn c [] "" 1)
(macro c [] "" 2)

(local d 1)
(macros {:d 2 :e 3})
