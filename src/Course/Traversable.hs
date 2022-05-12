{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.Traversable where

import Course.Applicative
import Course.Compose
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional

-- | All instances of the `Traversable` type-class must satisfy three laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t => Traversable t where
  traverse ::
    Applicative k =>
    (a -> k b) ->
    t a ->
    k (t b)

instance Traversable List where
  traverse ::
    Applicative k =>
    (a -> k b) ->
    List a ->
    k (List b)
  traverse f =
    foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse ::
    Applicative k =>
    (a -> k b) ->
    ExactlyOne a ->
    k (ExactlyOne b)
  traverse f (ExactlyOne a) =
    ExactlyOne <$> f a

instance Traversable Optional where
  traverse ::
    Applicative k =>
    (a -> k b) ->
    Optional a ->
    k (Optional b)
  traverse f (Full a) =
    Full <$> f a
  traverse _ _ =
    pure Empty

-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA ::
  (Applicative k, Traversable t) =>
  t (k a) ->
  k (t a)
sequenceA =
  traverse id

instance
  (Traversable f, Traversable g) =>
  Traversable (Compose f g)
  where
  -- Implement the traverse function for a Traversable instance for Compose
  traverse f (Compose fg) =
    Compose <$> traverse (traverse f) fg

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a
  = Product (f a) (g a)
  deriving (Show, Eq)

instance
  (Functor f, Functor g) =>
  Functor (Product f g)
  where
  -- Implement the (<$>) function for a Functor instance for Product
  k <$> (Product f g) =
    Product (k <$> f) (k <$> g)

instance
  (Traversable f, Traversable g) =>
  Traversable (Product f g)
  where
  -- Implement the traverse function for a Traversable instance for Product
  traverse k (Product f g) =
    lift2 Product (traverse k f) (traverse k g)

-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a
  = InL (f a)
  | InR (g a)
  deriving (Show, Eq)

instance
  (Functor f, Functor g) =>
  Functor (Coproduct f g)
  where
  -- Implement the (<$>) function for a Functor instance for Coproduct
  k <$> (InL fa) =
    InL (k <$> fa)
  k <$> (InR ga) =
    InR (k <$> ga)

instance
  (Traversable f, Traversable g) =>
  Traversable (Coproduct f g)
  where
  -- Implement the traverse function for a Traversable instance for Coproduct
  traverse k (InL fa) =
    InL <$> traverse k fa
  traverse k (InR ga) =
    InR <$> traverse k ga
