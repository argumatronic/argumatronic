-- "contravariant functors reverse the direction of composition"
-- remembering the functor (covariant) of functions is function composition, so this makes some prima facie sense.

-- ok normal composition looks like this:

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)




-- so we could write a sort of "contravariant function composition"
contraComp :: (a -> b) -> (b -> c) -> a -> c
contraComp f g x = g (f x)

-- yes, ok:

--λ> comp (+3) (*4) $ 10
--43
--it :: Num c => c

--λ> contraComp (+3) (*4) $ 10
--52
--it :: Num c => c

-- which doesn't even look like a big deal! but when we hide what's going on -- excuse me, ABSTRACT what's going on -- into the `f b -> f a` that looks superficially very similar to fmap -- well it *is* similar to fmap but it's similar to the *functor of functions* not to the functors of other types -- i don't really know how else to precisely distinguish the function type from other types?

-- now we compare this to what we know about contramap:

-- contramap : (a -> b) -> f b -> f a
--  (Int -> String) -> (String -> Bool) -> (Int -> Bool)
--   a        b         b          c        a       c
-- f b ~ (b -> c)
-- f a ~ (a -> c)

-- (a -> b) -> (b -> c) -> (a -> c)

-- f ~ (_ -> c)  ok i find this weird because both (a -> c) and (b -> c) are `f` and usually, e.g., with fmap and <*> etc, the two `f`s very much have to be the same thing -- i mean, i guess they are here and applying `f` to either `a` or `b` is how they vary. right. right right.

-- we talk too much about functors as "applying a function to some wrapped data" (i am absolutely guilty of this) and rely on intuitions about `map` but we should be thinking more about *composition* when we talk about functors.


-- Chris:

--Yeah comp and contraComp look good. Just to rephrase/reiterate, by looking at each type once they're been partially applied with one argument:

--In comp you get (a -> b) -> (a -> c) - a change of the return type
--In contraComp you get (b -> c) -> (a -> c) - a change of the parameter type

-- This might actually be a good pedagogical motivation for talking about contravariant functors, because they serve as an example that illustrates why "wrapped data" is a bad way to think about functors - because not only is the type parameter not "some wrapped data," it's the opposite of that.
