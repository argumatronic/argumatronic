monoids: conjunction, disjunction, commutativity. oh shit. i'd noticed before 

i don''t guess the other list monoid is commutative either. i guess that would be hard to do with lists.
and other such things
Chris Martin 
I think zip list is if the element monoid is
hmmm
yeah i guess it'd have to be. i had to think about what it looks like
but zip isn't. 
zip being the conjunctive/product list monoid. but i think zipWith zeroes out in the same way as zip. 
oh it was actually zip/zipWith that got me to thinking about conjunction and disjunction in monoids way back when, i forgot. it got bma real nervous.
actually is zip a monoid? idk what's the identity for that
what's a 1 for a list
Chris Martin 
wasn't alex talking about a "true" product type that was like (,) but unordered
yeh
Chris Martin 
so zip could commute with that?
I don't think list belongs to anything that has a 1?
no i don't think so either, but for a product you need a 1 not a 0, and an empty list is a 0
Chris Martin 
maybe [mempty] or repeat mempty in some circumstance?
well that does allow you to get something back other than an empty list
yeah but it stops after the first element
in the first list
huh
i guess that makes sense but i never thought about it before
λ> zipWith (++) ["julie", "chris"] [mempty]
["julie"]
it :: [[Char]]

λ> zipWith (++) ["julie", "chris"] mempty
[]
it :: [[Char]]
Sum Integer and so forth do the same. 

λ> ZipList [(+1), (+2)] <*> ZipList [3, 4, 5]
ZipList {getZipList = [4,6]}
it :: Num b => ZipList b

λ> ZipList [(+1), (*8)] <*> ZipList []
ZipList {getZipList = []}
it :: Num b => ZipList b

λ> ZipList [(++ "moronuki")] <*> ZipList [mempty]
ZipList {getZipList = ["moronuki"]}
it :: ZipList [Char]

λ> ZipList [(++ "moronuki")] <*> ZipList ["julie"]
ZipList {getZipList = ["juliemoronuki"]}
it :: ZipList [Char]

λ> ZipList [(++ "moronuki"), (++ "martin")] <*> ZipList ["julie"]
ZipList {getZipList = ["juliemoronuki"]}
it :: ZipList [Char]

λ> ZipList [(++ "moronuki"), (++ "martin")] <*> ZipList [mempty]
ZipList {getZipList = ["moronuki"]}
it :: ZipList [Char]

λ> zip [1, 2, 3] mempty
[]
it :: Num a => [(a, b)]

λ> zip [1, 2, 3] [mempty]
[(1,())]
it :: (Monoid b, Num a) => [(a, b)]

λ> zip "julie" mempty
[]
it :: [(Char, b)]

λ> zip "julie" "mempty"
[('j','m'),('u','e'),('l','m'),('i','p'),('e','t')]
it :: [(Char, Char)]
(LOL)

λ> zip "julie" [mempty]
[('j',())]
it :: Monoid b => [(Char, b)]

λ> zipWith (++) ["julie"] [mempty]
["julie"]
it :: [[Char]]
