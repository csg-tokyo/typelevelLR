
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Example2 where

-------------------------------------------------------------------------------

-- grammar definition

-- Add : E -> E "add()" T
-- TToE : E -> T
-- Num : F -> "num(Int)"
-- Paren : F -> "lp()" E "rp()"
-- Mul : T -> T "mul()" F
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
  = Num Int
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

class NumTransition s t | s -> t where
  num :: s -> Int -> Program r t

class LpTransition s t | s -> t where
  lp :: s -> Program r t

class RpTransition s t | s -> t where
  rp :: s -> Program r t

class MulTransition s t | s -> t where
  mul :: s -> Program r t

-------------------------------------------------------------------------------

-- automaton states

data Node1 prev = Node1 prev

data Node2 prev = Node2 prev E

data Node3 prev = Node3 prev T

data Node4 prev = Node4 prev

data Node5 prev = Node5 prev

data Node6 prev = Node6 prev E

data Node7 prev = Node7 prev F

data Node8 prev = Node8 prev F

data Node9 prev = Node9 prev

data Node10 prev = Node10 prev T

data Node11 prev = Node11 prev Int

data Node12 prev = Node12 prev

-------------------------------------------------------------------------------

-- shift transitions

instance {-# OVERLAPS #-} AddTransition (Node2 prev) (Node5 (Node2 prev)) where
  add prev = program (Node5 prev)

instance {-# OVERLAPS #-} AddTransition (Node6 prev) (Node5 (Node6 prev)) where
  add prev = program (Node5 prev)


instance {-# OVERLAPS #-} LpTransition (Node1 prev) (Node4 (Node1 prev)) where
  lp prev = program (Node4 prev)

instance {-# OVERLAPS #-} LpTransition (Node4 prev) (Node4 (Node4 prev)) where
  lp prev = program (Node4 prev)

instance {-# OVERLAPS #-} LpTransition (Node5 prev) (Node4 (Node5 prev)) where
  lp prev = program (Node4 prev)

instance {-# OVERLAPS #-} LpTransition (Node9 prev) (Node4 (Node9 prev)) where
  lp prev = program (Node4 prev)


instance {-# OVERLAPS #-} MulTransition (Node3 prev) (Node9 (Node3 prev)) where
  mul prev = program (Node9 prev)

instance {-# OVERLAPS #-} MulTransition (Node10 prev) (Node9 (Node10 prev)) where
  mul prev = program (Node9 prev)


instance {-# OVERLAPS #-} NumTransition (Node1 prev) (Node11 (Node1 prev)) where
  num prev x1 = program (Node11 prev x1)

instance {-# OVERLAPS #-} NumTransition (Node4 prev) (Node11 (Node4 prev)) where
  num prev x1 = program (Node11 prev x1)

instance {-# OVERLAPS #-} NumTransition (Node5 prev) (Node11 (Node5 prev)) where
  num prev x1 = program (Node11 prev x1)

instance {-# OVERLAPS #-} NumTransition (Node9 prev) (Node11 (Node9 prev)) where
  num prev x1 = program (Node11 prev x1)


instance {-# OVERLAPS #-} RpTransition (Node6 prev) (Node12 (Node6 prev)) where
  rp prev = program (Node12 prev)

-------------------------------------------------------------------------------

-- reduces

class Reduce s t | s -> t where
  reduce :: s -> t


-- Add : E -> E "add()" T

instance Reduce (Node3 (Node5 (Node2 (Node1 prev)))) (Node2 (Node1 prev)) where
  reduce (Node3 (Node5 (Node2 prev x1)) x2) = (Node2 prev (Add x1 x2))

instance Reduce (Node3 (Node5 (Node6 (Node4 prev)))) (Node6 (Node4 prev)) where
  reduce (Node3 (Node5 (Node6 prev x1)) x2) = (Node6 prev (Add x1 x2))


-- TToE : E -> T

instance Reduce (Node10 (Node1 prev)) (Node2 (Node1 prev)) where
  reduce (Node10 prev x1) = (Node2 prev (TToE x1))

instance Reduce (Node10 (Node4 prev)) (Node6 (Node4 prev)) where
  reduce (Node10 prev x1) = (Node6 prev (TToE x1))


-- Num : F -> "num(Int)"

instance Reduce (Node11 (Node1 prev)) (Node7 (Node1 prev)) where
  reduce (Node11 prev x1) = (Node7 prev (Num x1))

instance Reduce (Node11 (Node4 prev)) (Node7 (Node4 prev)) where
  reduce (Node11 prev x1) = (Node7 prev (Num x1))

instance Reduce (Node11 (Node5 prev)) (Node7 (Node5 prev)) where
  reduce (Node11 prev x1) = (Node7 prev (Num x1))

instance Reduce (Node11 (Node9 prev)) (Node8 (Node9 prev)) where
  reduce (Node11 prev x1) = (Node8 prev (Num x1))


-- Paren : F -> "lp()" E "rp()"

instance Reduce (Node12 (Node6 (Node4 (Node1 prev)))) (Node7 (Node1 prev)) where
  reduce (Node12 (Node6 (Node4 prev) x1)) = (Node7 prev (Paren x1))

instance Reduce (Node12 (Node6 (Node4 (Node4 prev)))) (Node7 (Node4 prev)) where
  reduce (Node12 (Node6 (Node4 prev) x1)) = (Node7 prev (Paren x1))

instance Reduce (Node12 (Node6 (Node4 (Node5 prev)))) (Node7 (Node5 prev)) where
  reduce (Node12 (Node6 (Node4 prev) x1)) = (Node7 prev (Paren x1))

instance Reduce (Node12 (Node6 (Node4 (Node9 prev)))) (Node8 (Node9 prev)) where
  reduce (Node12 (Node6 (Node4 prev) x1)) = (Node8 prev (Paren x1))


-- Mul : T -> T "mul()" F

instance Reduce (Node8 (Node9 (Node10 (Node1 prev)))) (Node10 (Node1 prev)) where
  reduce (Node8 (Node9 (Node10 prev x1)) x2) = (Node10 prev (Mul x1 x2))

instance Reduce (Node8 (Node9 (Node10 (Node4 prev)))) (Node10 (Node4 prev)) where
  reduce (Node8 (Node9 (Node10 prev x1)) x2) = (Node10 prev (Mul x1 x2))

instance Reduce (Node8 (Node9 (Node3 (Node5 prev)))) (Node3 (Node5 prev)) where
  reduce (Node8 (Node9 (Node3 prev x1)) x2) = (Node3 prev (Mul x1 x2))


-- FToT : T -> F

instance Reduce (Node7 (Node1 prev)) (Node10 (Node1 prev)) where
  reduce (Node7 prev x1) = (Node10 prev (FToT x1))

instance Reduce (Node7 (Node4 prev)) (Node10 (Node4 prev)) where
  reduce (Node7 prev x1) = (Node10 prev (FToT x1))

instance Reduce (Node7 (Node5 prev)) (Node3 (Node5 prev)) where
  reduce (Node7 prev x1) = (Node3 prev (FToT x1))


-- Reduce -> Transition

instance {-# OVERLAPS #-} (Reduce r s, AddTransition s t) => AddTransition r t where
  add r = add (reduce r)

instance {-# OVERLAPS #-} (Reduce r s, NumTransition s t) => NumTransition r t where
  num r x1 = num (reduce r) x1

instance {-# OVERLAPS #-} (Reduce r s, LpTransition s t) => LpTransition r t where
  lp r = lp (reduce r)

instance {-# OVERLAPS #-} (Reduce r s, RpTransition s t) => RpTransition r t where
  rp r = rp (reduce r)

instance {-# OVERLAPS #-} (Reduce r s, MulTransition s t) => MulTransition r t where
  mul r = mul (reduce r)

-------------------------------------------------------------------------------

-- initial state

begin :: Program r (Node1 ())
begin = program (Node1 ())

-------------------------------------------------------------------------------

class End s r | s -> r where
  end :: s -> r

instance {-# OVERLAPS #-} End (Node2 prev) E where
  end (Node2 _ r) = r

instance {-# OVERLAPS #-} (Reduce r s, End s t) => End r t where
  end r = end (reduce r)

-------------------------------------------------------------------------------

