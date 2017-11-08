
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Example1 where

-------------------------------------------------------------------------------

-- grammar definition

-- genA : A -> "a" A
-- BToA : A -> B
-- genAB : B -> "a" B "b"
-- fin : B -> eps

-------------------------------------------------------------------------------

type Program r a = (a -> r) -> r

program :: a -> Program r a
program a = \k -> k a

-------------------------------------------------------------------------------

-- AST nodes

data A
  = GenA A
  | BToA B
  deriving (Show)

data B
  = GenAB B
  | Fin
  deriving (Show)

-------------------------------------------------------------------------------

-- terminal symbols

class ATransition s t | s -> t where
  a :: s -> Program r t

class BTransition s t | s -> t where
  b :: s -> Program r t


-- automaton states

data S1 prev = S1 prev A

data S2 prev = S2 prev

data S3 prev = S3 prev B

data S4 prev = S4 prev B

data S5 prev = S5 prev

data S6 prev = S6 prev A

data S7 prev = S7 prev

-------------------------------------------------------------------------------

-- shift transitions

instance {-# OVERLAPS #-} ATransition (S2 prev) (S5 (S2 prev)) where
  a prev = program (S5 prev)


instance {-# OVERLAPS #-} BTransition (S4 prev) (S7 (S4 prev)) where
  b prev = program (S7 prev)


instance {-# OVERLAPS #-} ATransition (S5 prev) (S5 (S5 prev)) where
  a prev = program (S5 prev)

-------------------------------------------------------------------------------

-- reduces

class Reduce s t | s -> t where
  reduce :: s -> t


instance Reduce (S3 (S2 prev)) (S1 (S2 prev)) where
  reduce (S3 prev p1) = S1 prev (BToA p1)

instance Reduce (S6 (S5 (S2 prev))) (S1 (S2 prev)) where
  reduce (S6 (S5 prev) p1) = S1 prev (GenA p1)

instance Reduce (S2 prev) (S3 (S2 prev)) where
  reduce prev = S3 prev (Fin)

instance Reduce (S7 (S4 (S5 (S2 prev)))) (S3 (S2 prev)) where
  reduce (S7 (S4 (S5 prev) p1)) = S3 prev (GenAB p1)

instance Reduce (S4 (S5 prev)) (S6 (S5 prev)) where
  reduce (S4 prev p1) = S6 prev (BToA p1)

instance Reduce (S6 (S5 (S5 prev))) (S6 (S5 prev)) where
  reduce (S6 (S5 prev) p1) = S6 prev (GenA p1)

instance Reduce (S5 prev) (S4 (S5 prev)) where
  reduce prev = S4 prev (Fin)

instance Reduce (S7 (S4 (S5 (S5 prev)))) (S4 (S5 prev)) where
  reduce (S7 (S4 (S5 prev) p1)) = S4 prev (GenAB p1)


-- Reduce -> Transition

instance {-# OVERLAPS #-} (Reduce r s, ATransition s t) =>
    ATransition r t where
  a r = a (reduce r)

instance {-# OVERLAPS #-} (Reduce r s, BTransition s t) =>
    BTransition r t where
  b r = b (reduce r)

-------------------------------------------------------------------------------

-- initial state

begin :: Program r (S2 ())
begin = program (S2 ())

-------------------------------------------------------------------------------

class End s r | s -> r where
  end :: s -> r

instance {-# OVERLAPS #-} End (S1 prev) A where
  end (S1 _ r) = r

instance {-# OVERLAPS #-} (Reduce r s, End s t) => End r t where
  end r = end (reduce r)

-------------------------------------------------------------------------------

