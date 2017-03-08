import Data.Monoid (First, Last, (<>))

-- making up some types so they won't conflict with anything else I have in scope

-- a fake Maybe
data Possibly a = LolNo | HeckYah a
                  deriving (Eq, Show)

instance Monoid a => Monoid (Possibly a) where
  mempty = LolNo
  LolNo `mappend` m = m
  m `mappend` LolNo = m
  HeckYah m1 `mappend` HeckYah m2 = HeckYah (m1 `mappend` m2)

instance Functor Possibly where
  fmap _ LolNo = LolNo
  fmap f (HeckYah a) = HeckYah (f a)

instance Applicative Possibly where
  pure a = LolNo -- bad idea but it compiles
  (<*>) LolNo _       = LolNo
  (<*>) _ LolNo                 = LolNo
  (<*>) (HeckYah f) (HeckYah a) = HeckYah (f a)

  -- I bet a law involving fmap will have to fail
  -- yes, here
  -- fmap f x = pure f <*> x
  -- which means, by the first thing I said above, we'd need
  -- fmap f x = Nothing <*> x
  -- therefore
  -- fmap f x = x


-- a fake Either
-- data Choose a b = This a | That b
--                   deriving (Eq, Show)
--
-- instance Functor (Choose a) where
--   fmap _ (This a) = This a
--   fmap f (That b) = That (f b)
--
-- instance Applicative (Choose a) where
--   pure a = That a
--   (<*>) (This a) _ = This a -- due to function not having a Show instance, this line seems irrelevant? GHC 7.10 used to print Left <function> at least. in standard (not contrived, toy) usage, you couldn't really get a function embedded in the Left anyway, i think, but still.
--   (<*>) _ (This a) = This a
--   (<*>) (That f) (That b) = That (f b)
  -- λ> pure (*8) <*> That 4
  -- This ()
  -- when `pure a = This mempty`

-- a fake tuple type
data Tuple a b = Tuple a b
                 deriving (Eq, Show)

instance Functor (Tuple a) where
  fmap f (Tuple a b) = Tuple a (f b)

instance Monoid a => Applicative (Tuple a) where
  pure b = Tuple mempty b
  (<*>) (Tuple a f) (Tuple a' b) = Tuple (a <> a') (f b)

data Initial a = Nope | Yep a
              deriving (Eq, Show)

instance Monoid a => Monoid (Initial a) where
  mempty = Yep mempty
  mappend Nope Nope = Nope
  mappend (Yep a) _ = Yep a
  mappend _ (Yep a) = Yep a

instance Functor Initial where
  fmap _ Nope = Nope
  fmap f (Yep a) = Yep (f a)

instance Applicative Initial where
  pure = Yep
  (<*>) _ Nope = Nope
  (<*>) Nope (Yep a) = Nope
  (<*>) (Yep f) (Yep a) = Yep (f a)
  -- right it can only be this way because there is monoidal structure
  -- but there is also the (a -> b) function. cool.

-- a fake Either
data Choose a b = This a | That b
                  deriving (Eq, Show)

instance Functor (Choose a) where
  fmap _ (This a) = This a
  fmap f (That b) = That (f b)

instance Applicative (Choose a) where
  -- pure _ = This mempty  -- this is possible, but, again, the `a` remains bound in the type constructor
  pure a = This a
  (<*>) (This a) _ = This a -- due to function not having a Show instance, this line seems irrelevant? GHC 7.10 used to print Left <function> at least. in standard (not contrived, toy) usage, you couldn't really get a function embedded in the Left anyway, i think, but still.
  (<*>) _ (This a) = This a
  (<*>) (That f) (That b) = That (f b)
