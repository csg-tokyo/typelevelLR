
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Expr where

-------------------------------------------------------------------------------

-- grammar definition

-- Add : E -> E "add()" T
-- TToE : E -> T
-- Num : F -> "num(Integer)"
-- Paren : F -> "lp()" E "rp()"
-- Mul : T -> T "mul()" F
-- FToT : T -> F

-------------------------------------------------------------------------------

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

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
  add :: s -> t

class NumTransition s t | s -> t where
  num :: Integer -> s -> t

class LpTransition s t | s -> t where
  lp :: s -> t

class RpTransition s t | s -> t where
  rp :: s -> t

class MulTransition s t | s -> t where
  mul :: s -> t

class EndTransition s t | s -> t where
  end :: s -> t

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

data Node11 prev = Node11 prev Integer

data Node12 prev = Node12 prev

-------------------------------------------------------------------------------

-- transition instances

instance AddTransition (Node2 prev) (Node5 (Node2 prev)) where
  add src = Node5 src

instance EndTransition (Node2 prev) E where
  end (Node2 _ arg1) = arg1

instance LpTransition (Node1 prev) (Node4 (Node1 prev)) where
  lp src = Node4 src

instance NumTransition (Node1 prev) (Node11 (Node1 prev)) where
  num arg1 src = Node11 src arg1

instance (AddTransition (Node2 (Node1 prev)) t) => AddTransition (Node3 (Node5 (Node2 (Node1 prev)))) t where
  add (Node3 (Node5 (Node2 prev arg1)) arg2) = add (Node2 prev (Add arg1 arg2))

instance (AddTransition (Node6 (Node4 prev)) t) => AddTransition (Node3 (Node5 (Node6 (Node4 prev)))) t where
  add (Node3 (Node5 (Node6 prev arg1)) arg2) = add (Node6 prev (Add arg1 arg2))

instance MulTransition (Node3 prev) (Node9 (Node3 prev)) where
  mul src = Node9 src

instance (RpTransition (Node2 (Node1 prev)) t) => RpTransition (Node3 (Node5 (Node2 (Node1 prev)))) t where
  rp (Node3 (Node5 (Node2 prev arg1)) arg2) = rp (Node2 prev (Add arg1 arg2))

instance (RpTransition (Node6 (Node4 prev)) t) => RpTransition (Node3 (Node5 (Node6 (Node4 prev)))) t where
  rp (Node3 (Node5 (Node6 prev arg1)) arg2) = rp (Node6 prev (Add arg1 arg2))

instance (EndTransition (Node2 (Node1 prev)) t) => EndTransition (Node3 (Node5 (Node2 (Node1 prev)))) t where
  end (Node3 (Node5 (Node2 prev arg1)) arg2) = end (Node2 prev (Add arg1 arg2))

instance (EndTransition (Node6 (Node4 prev)) t) => EndTransition (Node3 (Node5 (Node6 (Node4 prev)))) t where
  end (Node3 (Node5 (Node6 prev arg1)) arg2) = end (Node6 prev (Add arg1 arg2))

instance LpTransition (Node4 prev) (Node4 (Node4 prev)) where
  lp src = Node4 src

instance NumTransition (Node4 prev) (Node11 (Node4 prev)) where
  num arg1 src = Node11 src arg1

instance LpTransition (Node5 prev) (Node4 (Node5 prev)) where
  lp src = Node4 src

instance NumTransition (Node5 prev) (Node11 (Node5 prev)) where
  num arg1 src = Node11 src arg1

instance AddTransition (Node6 prev) (Node5 (Node6 prev)) where
  add src = Node5 src

instance RpTransition (Node6 prev) (Node12 (Node6 prev)) where
  rp src = Node12 src

instance (AddTransition (Node10 (Node1 prev)) t) => AddTransition (Node7 (Node1 prev)) t where
  add (Node7 prev arg1) = add (Node10 prev (FToT arg1))

instance (AddTransition (Node10 (Node4 prev)) t) => AddTransition (Node7 (Node4 prev)) t where
  add (Node7 prev arg1) = add (Node10 prev (FToT arg1))

instance (AddTransition (Node3 (Node5 prev)) t) => AddTransition (Node7 (Node5 prev)) t where
  add (Node7 prev arg1) = add (Node3 prev (FToT arg1))

instance (MulTransition (Node10 (Node1 prev)) t) => MulTransition (Node7 (Node1 prev)) t where
  mul (Node7 prev arg1) = mul (Node10 prev (FToT arg1))

instance (MulTransition (Node10 (Node4 prev)) t) => MulTransition (Node7 (Node4 prev)) t where
  mul (Node7 prev arg1) = mul (Node10 prev (FToT arg1))

instance (MulTransition (Node3 (Node5 prev)) t) => MulTransition (Node7 (Node5 prev)) t where
  mul (Node7 prev arg1) = mul (Node3 prev (FToT arg1))

instance (RpTransition (Node10 (Node1 prev)) t) => RpTransition (Node7 (Node1 prev)) t where
  rp (Node7 prev arg1) = rp (Node10 prev (FToT arg1))

instance (RpTransition (Node10 (Node4 prev)) t) => RpTransition (Node7 (Node4 prev)) t where
  rp (Node7 prev arg1) = rp (Node10 prev (FToT arg1))

instance (RpTransition (Node3 (Node5 prev)) t) => RpTransition (Node7 (Node5 prev)) t where
  rp (Node7 prev arg1) = rp (Node3 prev (FToT arg1))

instance (EndTransition (Node10 (Node1 prev)) t) => EndTransition (Node7 (Node1 prev)) t where
  end (Node7 prev arg1) = end (Node10 prev (FToT arg1))

instance (EndTransition (Node10 (Node4 prev)) t) => EndTransition (Node7 (Node4 prev)) t where
  end (Node7 prev arg1) = end (Node10 prev (FToT arg1))

instance (EndTransition (Node3 (Node5 prev)) t) => EndTransition (Node7 (Node5 prev)) t where
  end (Node7 prev arg1) = end (Node3 prev (FToT arg1))

instance (AddTransition (Node10 (Node1 prev)) t) => AddTransition (Node8 (Node9 (Node10 (Node1 prev)))) t where
  add (Node8 (Node9 (Node10 prev arg1)) arg2) = add (Node10 prev (Mul arg1 arg2))

instance (AddTransition (Node10 (Node4 prev)) t) => AddTransition (Node8 (Node9 (Node10 (Node4 prev)))) t where
  add (Node8 (Node9 (Node10 prev arg1)) arg2) = add (Node10 prev (Mul arg1 arg2))

instance (AddTransition (Node3 (Node5 prev)) t) => AddTransition (Node8 (Node9 (Node3 (Node5 prev)))) t where
  add (Node8 (Node9 (Node3 prev arg1)) arg2) = add (Node3 prev (Mul arg1 arg2))

instance (MulTransition (Node10 (Node1 prev)) t) => MulTransition (Node8 (Node9 (Node10 (Node1 prev)))) t where
  mul (Node8 (Node9 (Node10 prev arg1)) arg2) = mul (Node10 prev (Mul arg1 arg2))

instance (MulTransition (Node10 (Node4 prev)) t) => MulTransition (Node8 (Node9 (Node10 (Node4 prev)))) t where
  mul (Node8 (Node9 (Node10 prev arg1)) arg2) = mul (Node10 prev (Mul arg1 arg2))

instance (MulTransition (Node3 (Node5 prev)) t) => MulTransition (Node8 (Node9 (Node3 (Node5 prev)))) t where
  mul (Node8 (Node9 (Node3 prev arg1)) arg2) = mul (Node3 prev (Mul arg1 arg2))

instance (RpTransition (Node10 (Node1 prev)) t) => RpTransition (Node8 (Node9 (Node10 (Node1 prev)))) t where
  rp (Node8 (Node9 (Node10 prev arg1)) arg2) = rp (Node10 prev (Mul arg1 arg2))

instance (RpTransition (Node10 (Node4 prev)) t) => RpTransition (Node8 (Node9 (Node10 (Node4 prev)))) t where
  rp (Node8 (Node9 (Node10 prev arg1)) arg2) = rp (Node10 prev (Mul arg1 arg2))

instance (RpTransition (Node3 (Node5 prev)) t) => RpTransition (Node8 (Node9 (Node3 (Node5 prev)))) t where
  rp (Node8 (Node9 (Node3 prev arg1)) arg2) = rp (Node3 prev (Mul arg1 arg2))

instance (EndTransition (Node10 (Node1 prev)) t) => EndTransition (Node8 (Node9 (Node10 (Node1 prev)))) t where
  end (Node8 (Node9 (Node10 prev arg1)) arg2) = end (Node10 prev (Mul arg1 arg2))

instance (EndTransition (Node10 (Node4 prev)) t) => EndTransition (Node8 (Node9 (Node10 (Node4 prev)))) t where
  end (Node8 (Node9 (Node10 prev arg1)) arg2) = end (Node10 prev (Mul arg1 arg2))

instance (EndTransition (Node3 (Node5 prev)) t) => EndTransition (Node8 (Node9 (Node3 (Node5 prev)))) t where
  end (Node8 (Node9 (Node3 prev arg1)) arg2) = end (Node3 prev (Mul arg1 arg2))

instance LpTransition (Node9 prev) (Node4 (Node9 prev)) where
  lp src = Node4 src

instance NumTransition (Node9 prev) (Node11 (Node9 prev)) where
  num arg1 src = Node11 src arg1

instance (AddTransition (Node2 (Node1 prev)) t) => AddTransition (Node10 (Node1 prev)) t where
  add (Node10 prev arg1) = add (Node2 prev (TToE arg1))

instance (AddTransition (Node6 (Node4 prev)) t) => AddTransition (Node10 (Node4 prev)) t where
  add (Node10 prev arg1) = add (Node6 prev (TToE arg1))

instance MulTransition (Node10 prev) (Node9 (Node10 prev)) where
  mul src = Node9 src

instance (RpTransition (Node2 (Node1 prev)) t) => RpTransition (Node10 (Node1 prev)) t where
  rp (Node10 prev arg1) = rp (Node2 prev (TToE arg1))

instance (RpTransition (Node6 (Node4 prev)) t) => RpTransition (Node10 (Node4 prev)) t where
  rp (Node10 prev arg1) = rp (Node6 prev (TToE arg1))

instance (EndTransition (Node2 (Node1 prev)) t) => EndTransition (Node10 (Node1 prev)) t where
  end (Node10 prev arg1) = end (Node2 prev (TToE arg1))

instance (EndTransition (Node6 (Node4 prev)) t) => EndTransition (Node10 (Node4 prev)) t where
  end (Node10 prev arg1) = end (Node6 prev (TToE arg1))

instance (AddTransition (Node7 (Node1 prev)) t) => AddTransition (Node11 (Node1 prev)) t where
  add (Node11 prev arg1) = add (Node7 prev (Num arg1))

instance (AddTransition (Node7 (Node4 prev)) t) => AddTransition (Node11 (Node4 prev)) t where
  add (Node11 prev arg1) = add (Node7 prev (Num arg1))

instance (AddTransition (Node7 (Node5 prev)) t) => AddTransition (Node11 (Node5 prev)) t where
  add (Node11 prev arg1) = add (Node7 prev (Num arg1))

instance (AddTransition (Node8 (Node9 prev)) t) => AddTransition (Node11 (Node9 prev)) t where
  add (Node11 prev arg1) = add (Node8 prev (Num arg1))

instance (MulTransition (Node7 (Node1 prev)) t) => MulTransition (Node11 (Node1 prev)) t where
  mul (Node11 prev arg1) = mul (Node7 prev (Num arg1))

instance (MulTransition (Node7 (Node4 prev)) t) => MulTransition (Node11 (Node4 prev)) t where
  mul (Node11 prev arg1) = mul (Node7 prev (Num arg1))

instance (MulTransition (Node7 (Node5 prev)) t) => MulTransition (Node11 (Node5 prev)) t where
  mul (Node11 prev arg1) = mul (Node7 prev (Num arg1))

instance (MulTransition (Node8 (Node9 prev)) t) => MulTransition (Node11 (Node9 prev)) t where
  mul (Node11 prev arg1) = mul (Node8 prev (Num arg1))

instance (RpTransition (Node7 (Node1 prev)) t) => RpTransition (Node11 (Node1 prev)) t where
  rp (Node11 prev arg1) = rp (Node7 prev (Num arg1))

instance (RpTransition (Node7 (Node4 prev)) t) => RpTransition (Node11 (Node4 prev)) t where
  rp (Node11 prev arg1) = rp (Node7 prev (Num arg1))

instance (RpTransition (Node7 (Node5 prev)) t) => RpTransition (Node11 (Node5 prev)) t where
  rp (Node11 prev arg1) = rp (Node7 prev (Num arg1))

instance (RpTransition (Node8 (Node9 prev)) t) => RpTransition (Node11 (Node9 prev)) t where
  rp (Node11 prev arg1) = rp (Node8 prev (Num arg1))

instance (EndTransition (Node7 (Node1 prev)) t) => EndTransition (Node11 (Node1 prev)) t where
  end (Node11 prev arg1) = end (Node7 prev (Num arg1))

instance (EndTransition (Node7 (Node4 prev)) t) => EndTransition (Node11 (Node4 prev)) t where
  end (Node11 prev arg1) = end (Node7 prev (Num arg1))

instance (EndTransition (Node7 (Node5 prev)) t) => EndTransition (Node11 (Node5 prev)) t where
  end (Node11 prev arg1) = end (Node7 prev (Num arg1))

instance (EndTransition (Node8 (Node9 prev)) t) => EndTransition (Node11 (Node9 prev)) t where
  end (Node11 prev arg1) = end (Node8 prev (Num arg1))

instance (AddTransition (Node7 (Node1 prev)) t) => AddTransition (Node12 (Node6 (Node4 (Node1 prev)))) t where
  add (Node12 (Node6 (Node4 prev) arg1)) = add (Node7 prev (Paren arg1))

instance (AddTransition (Node7 (Node4 prev)) t) => AddTransition (Node12 (Node6 (Node4 (Node4 prev)))) t where
  add (Node12 (Node6 (Node4 prev) arg1)) = add (Node7 prev (Paren arg1))

instance (AddTransition (Node7 (Node5 prev)) t) => AddTransition (Node12 (Node6 (Node4 (Node5 prev)))) t where
  add (Node12 (Node6 (Node4 prev) arg1)) = add (Node7 prev (Paren arg1))

instance (AddTransition (Node8 (Node9 prev)) t) => AddTransition (Node12 (Node6 (Node4 (Node9 prev)))) t where
  add (Node12 (Node6 (Node4 prev) arg1)) = add (Node8 prev (Paren arg1))

instance (MulTransition (Node7 (Node1 prev)) t) => MulTransition (Node12 (Node6 (Node4 (Node1 prev)))) t where
  mul (Node12 (Node6 (Node4 prev) arg1)) = mul (Node7 prev (Paren arg1))

instance (MulTransition (Node7 (Node4 prev)) t) => MulTransition (Node12 (Node6 (Node4 (Node4 prev)))) t where
  mul (Node12 (Node6 (Node4 prev) arg1)) = mul (Node7 prev (Paren arg1))

instance (MulTransition (Node7 (Node5 prev)) t) => MulTransition (Node12 (Node6 (Node4 (Node5 prev)))) t where
  mul (Node12 (Node6 (Node4 prev) arg1)) = mul (Node7 prev (Paren arg1))

instance (MulTransition (Node8 (Node9 prev)) t) => MulTransition (Node12 (Node6 (Node4 (Node9 prev)))) t where
  mul (Node12 (Node6 (Node4 prev) arg1)) = mul (Node8 prev (Paren arg1))

instance (RpTransition (Node7 (Node1 prev)) t) => RpTransition (Node12 (Node6 (Node4 (Node1 prev)))) t where
  rp (Node12 (Node6 (Node4 prev) arg1)) = rp (Node7 prev (Paren arg1))

instance (RpTransition (Node7 (Node4 prev)) t) => RpTransition (Node12 (Node6 (Node4 (Node4 prev)))) t where
  rp (Node12 (Node6 (Node4 prev) arg1)) = rp (Node7 prev (Paren arg1))

instance (RpTransition (Node7 (Node5 prev)) t) => RpTransition (Node12 (Node6 (Node4 (Node5 prev)))) t where
  rp (Node12 (Node6 (Node4 prev) arg1)) = rp (Node7 prev (Paren arg1))

instance (RpTransition (Node8 (Node9 prev)) t) => RpTransition (Node12 (Node6 (Node4 (Node9 prev)))) t where
  rp (Node12 (Node6 (Node4 prev) arg1)) = rp (Node8 prev (Paren arg1))

instance (EndTransition (Node7 (Node1 prev)) t) => EndTransition (Node12 (Node6 (Node4 (Node1 prev)))) t where
  end (Node12 (Node6 (Node4 prev) arg1)) = end (Node7 prev (Paren arg1))

instance (EndTransition (Node7 (Node4 prev)) t) => EndTransition (Node12 (Node6 (Node4 (Node4 prev)))) t where
  end (Node12 (Node6 (Node4 prev) arg1)) = end (Node7 prev (Paren arg1))

instance (EndTransition (Node7 (Node5 prev)) t) => EndTransition (Node12 (Node6 (Node4 (Node5 prev)))) t where
  end (Node12 (Node6 (Node4 prev) arg1)) = end (Node7 prev (Paren arg1))

instance (EndTransition (Node8 (Node9 prev)) t) => EndTransition (Node12 (Node6 (Node4 (Node9 prev)))) t where
  end (Node12 (Node6 (Node4 prev) arg1)) = end (Node8 prev (Paren arg1))

-------------------------------------------------------------------------------

begin :: Node1 ()
begin = Node1 ()

-------------------------------------------------------------------------------

