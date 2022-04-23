{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module UI.Utils (module UI.Utils, toList) where

import Prelude hiding (foldr)

import Data.Foldable (foldr, toList)

import Control.Applicative

--- Dependently typed static size list ----

data Size = Z | S Size

-- ExplicitSizedList is a list with a staticallu typed size
data ExplicitSizedList :: Size -> * -> * where
  E :: ExplicitSizedList 'Z a
  (:.) :: a -> ExplicitSizedList n a -> ExplicitSizedList ('S n) a

infixr 1 :.

instance Functor (ExplicitSizedList n) where
    fmap _ E = E
    fmap f (x:.xs) = f x :. fmap f xs

instance Traversable (ExplicitSizedList n) where
    traverse :: Applicative f => (a -> f b) -> ExplicitSizedList n a -> f (ExplicitSizedList n b)
    traverse f (x:.xs) = liftA2 (:.) (f x) (traverse f xs)
    traverse _ E = pure E

instance Foldable (ExplicitSizedList n) where
    foldr :: (a -> b -> b) -> b -> ExplicitSizedList n a -> b
    foldr _ z E = z
    foldr f' z (x:.xs) = f' x (foldr f' z xs)

-- -- | Fields is a dependently typed datatype containing a fixed amount of fields
-- -- Which is used in the forms functions, which takes a variable list of labels,
-- -- but returns a fixed amount of fields, which can be pattern matched against
-- -- exactly
-- data SizedList :: * -> * where
--     SizedList :: ExplicitSizedList n a -> SizedList a

-- instance Functor SizedList where
--     fmap f (SizedList l) = SizedList (fmap f l)

-- instance Traversable SizedList where
--     traverse f (SizedList x) = SizedList <$> traverse f x

-- instance Foldable SizedList where
--     foldr :: (a -> b -> b) -> b -> SizedList a -> b
--     foldr f z (SizedList l) = foldr f z l

-- fromList :: [a] -> SizedList a
-- fromList []     = SizedList E
-- fromList (x:xs) = case fromList xs of
--     SizedList l -> SizedList (x :. l)

-- -- | Convert a list of 'Dynamic's into a 'Dynamic' list.
-- distributeSizedListOverDyn :: Reflex t => ExplicitSizedList n (Dynamic t a) -> Dynamic t (ExplicitSizedList n a)
-- distributeSizedListOverDyn = distributeSizedListOverDynWith id

-- -- | Create a new 'Dynamic' by applying a combining function to a list of 'Dynamic's
-- distributeSizedListOverDynWith :: Reflex t => (ExplicitSizedList n a -> b) -> ExplicitSizedList n (Dynamic t a) -> Dynamic t b
-- distributeSizedListOverDynWith f =
--   fmap (f . fmap fromDSum . toAscSizedList) .
--   distributeDMapOverDynPure .
--   fromDistinctAscSizedList .
--   zipSizedWith toDSum (undefined [0..])
--   where
--     toDSum :: Int -> Dynamic t a -> DSum (Const2 Int a) (Dynamic t)
--     toDSum k v = Const2 k :=> v
--     fromDSum :: DSum (Const2 Int a) Identity -> a
--     fromDSum (Const2 _ :=> Identity v) = v
--     toAscSizedList :: DMap.DMap k f -> ExplicitSizedList n (DSum k f)
--     toAscSizedList t = undefined -- DMap.foldrWithKey (\k x xs -> (k :=> x):.xs) E t

--     fromDistinctAscSizedList :: ExplicitSizedList n (DSum k2 f) -> DMap.DMap k2 f
--     fromDistinctAscSizedList = build const (length xs) xs
--       where
--         -- 1) use continutations so that we use heap space instead of stack space.
--         -- 2) special case for n==5 to build bushier trees.

--         build :: (DMap.DMap k f -> [DSum k f] -> b) -> Int -> [DSum k f] -> b
--         build c 0 xs'  = c Tip xs'
--         build c 5 xs'  = case xs' of
--                            ((k1:=>x1):(k2:=>x2):(k3:=>x3):(k4:=>x4):(k5:=>x5):xx)
--                                 -> c (bin k4 x4 (bin k2 x2 (singleton k1 x1) (singleton k3 x3)) (singleton k5 x5)) xx
--                            _ -> error "fromDistinctAscList build"
--         build c n xs'  = seq nr $ build (buildR nr c) nl xs'
--                        where
--                          nl = n `div` 2
--                          nr = n - nl - 1

--         buildR :: Int -> (DMap.DMap k f -> [DSum k f] -> b) -> DMap.DMap k f -> [DSum k f] -> b
--         buildR n c l ((k:=>x):ys) = build (buildB l k x c) n ys
--         buildR _ _ _ []           = error "fromDistinctAscList buildR []"

--         buildB :: DMap.DMap k f -> k v -> f v -> (DMap.DMap k f -> a -> b) -> DMap.DMap k f -> a -> b
--         buildB l k x c r zs       = c (bin k x l r) zs

--     zipSizedWith :: (a -> b -> c) -> ExplicitSizedList n a -> ExplicitSizedList n b -> ExplicitSizedList n c
--     zipSizedWith _ E _ = E
--     zipSizedWith f' (x:.xs) (y:.ys) = f' x y :. zipSizedWith f' xs ys

-- -- Number which also has a statically typed size
-- data SizedNat :: Size -> * where
--   NZ :: SizedNat 'Z
--   NS :: SizedNat n -> SizedNat ('S n)

------------------------------------------
