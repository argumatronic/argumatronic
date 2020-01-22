genumRep :: (GEnum a, Ord b) => (a -> b) -> Rep a b
genumRep f =
  let
    reverseMap = foldMap (\a -> Map.singleton (f a) a) genum
  in
    Rep
      { encode = \a -> f a
      , decode = \b -> Map.lookup b reverseMap
      }

main =
  do
    let
        productId x = genumRep
         (case x of
            Basic    -> "basic"
            Standard -> "normal"
            Super    -> "super")

    putStrLn (encode productId Basic)
    putStrLn (encode productId Standard)

    -- so when we `encode productId x`
    -- wtf i don't remember now what this was for