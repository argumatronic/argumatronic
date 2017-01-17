monoids: conjunction, disjunction, commutativity. oh shit. i'd noticed before that it's one type of type (i think higher-kinded) that -- if it's a kind * type then the monoid can only refer to itself, but with higher-kinded types it can refer to the type inside so the number of possible monoids proliferates. so like with Bool and Integer you get basic conjunctive/disjunctive monoids (oh, but also list??) but then with Maybe you get more based on whether you conjoin the `a` value or disjoin the outer layer. right? huh

lists are interesting because 

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

-- can list even have a 1 value, for a conjunctive identity? chris suggests an infinite list of the mempty for the values inside with a Monoid constraint that would actually mappend the values -- so he might be right about that, if you could construct such a value for a mempty?

f :: (Monoid a) => [a] -> [a] -> [(a, a)]
f xs ys = zip xs ys

λ> f ["julie", "chris"] [mempty, mempty]
[("julie",""),("chris","")]

-- so this one doesn't do it because it doesn't actually use the monoid instance to concatenate the lists -- need to write the zip function differently. or zipWith the mappend?

λ> zipWith mappend ["julie", "Chris"] ["moronuki", "martin"]
["juliemoronuki","Chrismartin"]

λ> zipWith mappend ["julie", "Chris"] [mempty, mempty]
["julie","Chris"]
it :: (Monoid c, Data.String.IsString c) => [c]

-- i'd also considered alist of Unit values, but i'm not sure how to make that work exactly. i think in theory it should work? possibly only in theory, not code. at any rate, then the question is whether this is a proper 1 value of a list -- a proper identity of a list for the terms of conjunction. compare it to what other 1 values for products look like (Maybe, etc) to decide that. there's also the issue of idk how to make a mempty value for the typeclass that is an infinite list of mempty. mempty isn't, like, enumerable.? i must be missing something dumb here but.

λ> zipWith mappend ([Any True, Any True]) ([Any True, Any False])
[Any {getAny = True},Any {getAny = True}]
it :: [Any]

λ> zipWith mappend ([Any True, Any True]) ([mempty, mempty])
[Any {getAny = True},Any {getAny = True}]
it :: [Any]

-- yes of course this is an adequate 1 identity, because the empty list is a zero because it contains zero values. this contains a value, but it's an identity value only. yes for the contents, but that's not relevant.