import Control.Applicative (ZipList)
import Data.Monoid (First, Last, (<>))
-- import Data.List (NonEmpty)

-- making up some types so they won't conflict with anything else I have in scope

-- a fake Maybe
data Possibly a = LolNo | HeckYah a
                  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNo = LolNo
  fmap f (HeckYah a) = HeckYah (f a)

instance Applicative Possibly where
  pure = HeckYah
  (<*>) LolNo _                 = LolNo
  (<*>) _ LolNo                 = LolNo
  (<*>) (HeckYah f) (HeckYah a) = HeckYah (f a)

-- hooray for a super contrived example:
-- λ> HeckYah toUpper <*> HeckYah 'j'
-- HeckYah 'J'

-- a fake Either
data Choose a b = This a | That b
                  deriving (Eq, Show)

instance Functor (Choose a) where
  fmap _ (This a) = This a
  fmap f (That b) = That (f b)

instance Applicative (Choose a) where
  pure = That
  (<*>) (This a) _ = This a
  (<*>) _ (This a) = This a
  (<*>) (That f) (That b) = That (f b)

-- a fake tuple type
data Tuple a b = Tuple a b
                 deriving (Eq, Show)

instance Functor (Tuple a) where
  fmap f (Tuple a b) = Tuple a (f b)

instance Monoid a => Applicative (Tuple a) where
  pure b = Tuple mempty b
  (<*>) (Tuple a f) (Tuple a' b) = Tuple (a <> a') (f b)
