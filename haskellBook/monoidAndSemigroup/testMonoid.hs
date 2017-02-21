module TestMonoid where
import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type S = String
type B = Bool
-- quickCheck (monoidAssoc :: S -> S -> S -> B)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- quickCheck (monoidLeftIdentity :: String -> Bool)
-- quickCheck (monoidRightIdentity :: String -> Bool)
