
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Example2 where

-------------------------------------------------------------------------------

-- grammar definition

-- Add : E -> E "add" T
-- TToE : E -> T
-- Num : F -> int
-- Paren : F -> "lp" E "rp"
-- Mul : T -> T "mul" F
-- FToT : T -> F

-------------------------------------------------------------------------------

type Program r a = (a -> r) -> r

program :: a -> Program r a
program a = \k -> k a

-------------------------------------------------------------------------------

-- AST nodes

data E
  = Add E T
  | TToE T
  deriving (Show)

data F
  = Num Integer
  | Paren E
  deriving (Show)

data T
  = Mul T F
  | FToT F
  deriving (Show)

-------------------------------------------------------------------------------

-- terminal symbols

class AddTransition s t | s -> t where
  add :: s -> Program r t

class LpTransition s t | s -> t where
  lp :: s -> Program r t

class MulTransition s t | s -> t where
  mul :: s -> Program r t

class RpTransition s t | s -> t where
  rp :: s -> Program r t

class IntTransition s t | s -> t where
  int :: s -> Integer -> Program r t


-- automaton states

data S1 prev = S1 prev E

data S2 prev = S2 prev

data S3 prev = S3 prev T

data S4 prev = S4 prev

data S5 prev = S5 prev

data S6 prev = S6 prev E

data S7 prev = S7 prev T

data S8 prev = S8 prev Integer

data S9 prev = S9 prev

data S10 prev = S10 prev

data S11 prev = S11 prev F

data S12 prev = S12 prev F

-------------------------------------------------------------------------------

-- shift transitions

instance {-# OVERLAPS #-} AddTransition (S1 prev) (S5 (S1 prev)) where
  add prev = program (S5 prev)


instance {-# OVERLAPS #-} LpTransition (S2 prev) (S4 (S2 prev)) where
  lp prev = program (S4 prev)

instance {-# OVERLAPS #-} IntTransition (S2 prev) (S8 (S2 prev)) where
  int prev i = program (S8 prev i)


instance {-# OVERLAPS #-} MulTransition (S3 prev) (S9 (S3 prev)) where
  mul prev = program (S9 prev)


instance {-# OVERLAPS #-} LpTransition (S4 prev) (S4 (S4 prev)) where
  lp prev = program (S4 prev)

instance {-# OVERLAPS #-} IntTransition (S4 prev) (S8 (S4 prev)) where
  int prev i = program (S8 prev i)


instance {-# OVERLAPS #-} LpTransition (S5 prev) (S4 (S5 prev)) where
  lp prev = program (S4 prev)

instance {-# OVERLAPS #-} IntTransition (S5 prev) (S8 (S5 prev)) where
  int prev i = program (S8 prev i)


instance {-# OVERLAPS #-} AddTransition (S6 prev) (S5 (S6 prev)) where
  add prev = program (S5 prev)

instance {-# OVERLAPS #-} RpTransition (S6 prev) (S10 (S6 prev)) where
  rp prev = program (S10 prev)


instance {-# OVERLAPS #-} MulTransition (S7 prev) (S9 (S7 prev)) where
  mul prev = program (S9 prev)


instance {-# OVERLAPS #-} LpTransition (S9 prev) (S4 (S9 prev)) where
  lp prev = program (S4 prev)

instance {-# OVERLAPS #-} IntTransition (S9 prev) (S8 (S9 prev)) where
  int prev i = program (S8 prev i)

-------------------------------------------------------------------------------

-- reduces

class Reduce s t | s -> t where
  reduce :: s -> t


instance Reduce (S3 (S5 (S1 (S2 prev)))) (S1 (S2 prev)) where
  reduce (S3 (S5 (S1 prev p1)) p2) = S1 prev (Add p1 p2)

instance Reduce (S7 (S2 prev)) (S1 (S2 prev)) where
  reduce (S7 prev p1) = S1 prev (TToE p1)

instance Reduce (S8 (S2 prev)) (S11 (S2 prev)) where
  reduce (S8 prev p1) = S11 prev (Num p1)

instance Reduce (S10 (S6 (S4 (S2 prev)))) (S11 (S2 prev)) where
  reduce (S10 (S6 (S4 prev) p1)) = S11 prev (Paren p1)

instance Reduce (S11 (S2 prev)) (S7 (S2 prev)) where
  reduce (S11 prev p1) = S7 prev (FToT p1)

instance Reduce (S12 (S9 (S7 (S2 prev)))) (S7 (S2 prev)) where
  reduce (S12 (S9 (S7 prev p1)) p2) = S7 prev (Mul p1 p2)

instance Reduce (S3 (S5 (S6 (S4 prev)))) (S6 (S4 prev)) where
  reduce (S3 (S5 (S6 prev p1)) p2) = S6 prev (Add p1 p2)

instance Reduce (S7 (S4 prev)) (S6 (S4 prev)) where
  reduce (S7 prev p1) = S6 prev (TToE p1)

instance Reduce (S8 (S4 prev)) (S11 (S4 prev)) where
  reduce (S8 prev p1) = S11 prev (Num p1)

instance Reduce (S10 (S6 (S4 (S4 prev)))) (S11 (S4 prev)) where
  reduce (S10 (S6 (S4 prev) p1)) = S11 prev (Paren p1)

instance Reduce (S11 (S4 prev)) (S7 (S4 prev)) where
  reduce (S11 prev p1) = S7 prev (FToT p1)

instance Reduce (S12 (S9 (S7 (S4 prev)))) (S7 (S4 prev)) where
  reduce (S12 (S9 (S7 prev p1)) p2) = S7 prev (Mul p1 p2)

instance Reduce (S8 (S5 prev)) (S11 (S5 prev)) where
  reduce (S8 prev p1) = S11 prev (Num p1)

instance Reduce (S10 (S6 (S4 (S5 prev)))) (S11 (S5 prev)) where
  reduce (S10 (S6 (S4 prev) p1)) = S11 prev (Paren p1)

instance Reduce (S11 (S5 prev)) (S3 (S5 prev)) where
  reduce (S11 prev p1) = S3 prev (FToT p1)

instance Reduce (S12 (S9 (S3 (S5 prev)))) (S3 (S5 prev)) where
  reduce (S12 (S9 (S3 prev p1)) p2) = S3 prev (Mul p1 p2)

instance Reduce (S8 (S9 prev)) (S12 (S9 prev)) where
  reduce (S8 prev p1) = S12 prev (Num p1)

instance Reduce (S10 (S6 (S4 (S9 prev)))) (S12 (S9 prev)) where
  reduce (S10 (S6 (S4 prev) p1)) = S12 prev (Paren p1)


-- Reduce -> Transition

instance {-# OVERLAPS #-} (Reduce r s, AddTransition s t) =>
    AddTransition r t where
  add r = add (reduce r)

instance {-# OVERLAPS #-} (Reduce r s, LpTransition s t) =>
    LpTransition r t where
  lp r = lp (reduce r)

instance {-# OVERLAPS #-} (Reduce r s, MulTransition s t) =>
    MulTransition r t where
  mul r = mul (reduce r)

instance {-# OVERLAPS #-} (Reduce r s, RpTransition s t) =>
    RpTransition r t where
  rp r = rp (reduce r)

instance {-# OVERLAPS #-} (Reduce r s, IntTransition s t) =>
    IntTransition r t where
  int r s = int (reduce r) s

-------------------------------------------------------------------------------

-- initial state

begin :: Program r (S2 ())
begin = program (S2 ())

-------------------------------------------------------------------------------

class End s r | s -> r where
  end :: s -> r

instance {-# OVERLAPS #-} End (S1 prev) E where
  end (S1 _ r) = r

instance {-# OVERLAPS #-} (Reduce r s, End s t) => End r t where
  end r = end (reduce r)

-------------------------------------------------------------------------------

