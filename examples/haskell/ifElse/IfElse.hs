
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module IfElse where

-------------------------------------------------------------------------------

-- grammar definition

-- S1 : S -> "a()" Expr "a()"
-- S2 : S -> "b()" Expr "b()"
-- ElseClause : Else -> "else_()" Expr
-- Return : Expr -> "return_(String)"
-- IfThen : Expr -> "if_(Bool)" Then
-- IfThenElse : Expr -> "if_(Bool)" Then Else
-- ThenClause : Then -> "then_()" Expr

-------------------------------------------------------------------------------

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

-------------------------------------------------------------------------------

-- AST nodes

data S
  = S1 Expr
  | S2 Expr
  deriving (Show)

data Else
  = ElseClause Expr
  deriving (Show)

data Expr
  = Return String
  | IfThen Bool Then
  | IfThenElse Bool Then Else
  deriving (Show)

data Then
  = ThenClause Expr
  deriving (Show)

-------------------------------------------------------------------------------

-- terminal symbols

class Else_Transition s t | s -> t where
  else_ :: s -> t

class Return_Transition s t | s -> t where
  return_ :: String -> s -> t

class If_Transition s t | s -> t where
  if_ :: Bool -> s -> t

class ATransition s t | s -> t where
  a :: s -> t

class BTransition s t | s -> t where
  b :: s -> t

class Then_Transition s t | s -> t where
  then_ :: s -> t

class EndTransition s t | s -> t where
  end :: s -> t

-------------------------------------------------------------------------------

-- automaton states

data Node1 prev = Node1 prev

data Node2 prev = Node2 prev S

data Node3 prev = Node3 prev Expr

data Node4 prev = Node4 prev

data Node5 prev = Node5 prev Then

data Node6 prev = Node6 prev Bool

data Node7 prev = Node7 prev

data Node8 prev = Node8 prev

data Node9 prev = Node9 prev

data Node10 prev = Node10 prev Else

data Node11 prev = Node11 prev String

data Node12 prev = Node12 prev

data Node13 prev = Node13 prev Expr

data Node14 prev = Node14 prev

data Node15 prev = Node15 prev Expr

data Node16 prev = Node16 prev Expr

-------------------------------------------------------------------------------

-- transition instances

instance EndTransition (Node2 prev) S where
  end (Node2 _ arg1) = arg1

instance ATransition (Node1 prev) (Node7 (Node1 prev)) where
  a src = Node7 src

instance BTransition (Node1 prev) (Node9 (Node1 prev)) where
  b src = Node9 src

instance (ATransition (Node10 (Node5 prev)) t) => ATransition (Node3 (Node4 (Node5 prev))) t where
  a (Node3 (Node4 prev) arg1) = a (Node10 prev (ElseClause arg1))

instance (BTransition (Node10 (Node5 prev)) t) => BTransition (Node3 (Node4 (Node5 prev))) t where
  b (Node3 (Node4 prev) arg1) = b (Node10 prev (ElseClause arg1))

instance (Else_Transition (Node10 (Node5 prev)) t) => Else_Transition (Node3 (Node4 (Node5 prev))) t where
  else_ (Node3 (Node4 prev) arg1) = else_ (Node10 prev (ElseClause arg1))

instance If_Transition (Node4 prev) (Node6 (Node4 prev)) where
  if_ arg1 src = Node6 src arg1

instance Return_Transition (Node4 prev) (Node11 (Node4 prev)) where
  return_ arg1 src = Node11 src arg1

instance (ATransition (Node3 (Node4 prev)) t) => ATransition (Node5 (Node6 (Node4 prev))) t where
  a (Node5 (Node6 prev arg1) arg2) = a (Node3 prev (IfThen arg1 arg2))

instance (ATransition (Node13 (Node7 prev)) t) => ATransition (Node5 (Node6 (Node7 prev))) t where
  a (Node5 (Node6 prev arg1) arg2) = a (Node13 prev (IfThen arg1 arg2))

instance (ATransition (Node16 (Node8 prev)) t) => ATransition (Node5 (Node6 (Node8 prev))) t where
  a (Node5 (Node6 prev arg1) arg2) = a (Node16 prev (IfThen arg1 arg2))

instance (ATransition (Node15 (Node9 prev)) t) => ATransition (Node5 (Node6 (Node9 prev))) t where
  a (Node5 (Node6 prev arg1) arg2) = a (Node15 prev (IfThen arg1 arg2))

instance (BTransition (Node3 (Node4 prev)) t) => BTransition (Node5 (Node6 (Node4 prev))) t where
  b (Node5 (Node6 prev arg1) arg2) = b (Node3 prev (IfThen arg1 arg2))

instance (BTransition (Node13 (Node7 prev)) t) => BTransition (Node5 (Node6 (Node7 prev))) t where
  b (Node5 (Node6 prev arg1) arg2) = b (Node13 prev (IfThen arg1 arg2))

instance (BTransition (Node16 (Node8 prev)) t) => BTransition (Node5 (Node6 (Node8 prev))) t where
  b (Node5 (Node6 prev arg1) arg2) = b (Node16 prev (IfThen arg1 arg2))

instance (BTransition (Node15 (Node9 prev)) t) => BTransition (Node5 (Node6 (Node9 prev))) t where
  b (Node5 (Node6 prev arg1) arg2) = b (Node15 prev (IfThen arg1 arg2))

instance Else_Transition (Node5 prev) (Node4 (Node5 prev)) where
  else_ src = Node4 src

instance Then_Transition (Node6 prev) (Node8 (Node6 prev)) where
  then_ src = Node8 src

instance If_Transition (Node7 prev) (Node6 (Node7 prev)) where
  if_ arg1 src = Node6 src arg1

instance Return_Transition (Node7 prev) (Node11 (Node7 prev)) where
  return_ arg1 src = Node11 src arg1

instance If_Transition (Node8 prev) (Node6 (Node8 prev)) where
  if_ arg1 src = Node6 src arg1

instance Return_Transition (Node8 prev) (Node11 (Node8 prev)) where
  return_ arg1 src = Node11 src arg1

instance If_Transition (Node9 prev) (Node6 (Node9 prev)) where
  if_ arg1 src = Node6 src arg1

instance Return_Transition (Node9 prev) (Node11 (Node9 prev)) where
  return_ arg1 src = Node11 src arg1

instance (ATransition (Node3 (Node4 prev)) t) => ATransition (Node10 (Node5 (Node6 (Node4 prev)))) t where
  a (Node10 (Node5 (Node6 prev arg1) arg2) arg3) = a (Node3 prev (IfThenElse arg1 arg2 arg3))

instance (ATransition (Node13 (Node7 prev)) t) => ATransition (Node10 (Node5 (Node6 (Node7 prev)))) t where
  a (Node10 (Node5 (Node6 prev arg1) arg2) arg3) = a (Node13 prev (IfThenElse arg1 arg2 arg3))

instance (ATransition (Node16 (Node8 prev)) t) => ATransition (Node10 (Node5 (Node6 (Node8 prev)))) t where
  a (Node10 (Node5 (Node6 prev arg1) arg2) arg3) = a (Node16 prev (IfThenElse arg1 arg2 arg3))

instance (ATransition (Node15 (Node9 prev)) t) => ATransition (Node10 (Node5 (Node6 (Node9 prev)))) t where
  a (Node10 (Node5 (Node6 prev arg1) arg2) arg3) = a (Node15 prev (IfThenElse arg1 arg2 arg3))

instance (BTransition (Node3 (Node4 prev)) t) => BTransition (Node10 (Node5 (Node6 (Node4 prev)))) t where
  b (Node10 (Node5 (Node6 prev arg1) arg2) arg3) = b (Node3 prev (IfThenElse arg1 arg2 arg3))

instance (BTransition (Node13 (Node7 prev)) t) => BTransition (Node10 (Node5 (Node6 (Node7 prev)))) t where
  b (Node10 (Node5 (Node6 prev arg1) arg2) arg3) = b (Node13 prev (IfThenElse arg1 arg2 arg3))

instance (BTransition (Node16 (Node8 prev)) t) => BTransition (Node10 (Node5 (Node6 (Node8 prev)))) t where
  b (Node10 (Node5 (Node6 prev arg1) arg2) arg3) = b (Node16 prev (IfThenElse arg1 arg2 arg3))

instance (BTransition (Node15 (Node9 prev)) t) => BTransition (Node10 (Node5 (Node6 (Node9 prev)))) t where
  b (Node10 (Node5 (Node6 prev arg1) arg2) arg3) = b (Node15 prev (IfThenElse arg1 arg2 arg3))

instance (Else_Transition (Node3 (Node4 prev)) t) => Else_Transition (Node10 (Node5 (Node6 (Node4 prev)))) t where
  else_ (Node10 (Node5 (Node6 prev arg1) arg2) arg3) = else_ (Node3 prev (IfThenElse arg1 arg2 arg3))

instance (Else_Transition (Node13 (Node7 prev)) t) => Else_Transition (Node10 (Node5 (Node6 (Node7 prev)))) t where
  else_ (Node10 (Node5 (Node6 prev arg1) arg2) arg3) = else_ (Node13 prev (IfThenElse arg1 arg2 arg3))

instance (Else_Transition (Node16 (Node8 prev)) t) => Else_Transition (Node10 (Node5 (Node6 (Node8 prev)))) t where
  else_ (Node10 (Node5 (Node6 prev arg1) arg2) arg3) = else_ (Node16 prev (IfThenElse arg1 arg2 arg3))

instance (Else_Transition (Node15 (Node9 prev)) t) => Else_Transition (Node10 (Node5 (Node6 (Node9 prev)))) t where
  else_ (Node10 (Node5 (Node6 prev arg1) arg2) arg3) = else_ (Node15 prev (IfThenElse arg1 arg2 arg3))

instance (ATransition (Node3 (Node4 prev)) t) => ATransition (Node11 (Node4 prev)) t where
  a (Node11 prev arg1) = a (Node3 prev (Return arg1))

instance (ATransition (Node13 (Node7 prev)) t) => ATransition (Node11 (Node7 prev)) t where
  a (Node11 prev arg1) = a (Node13 prev (Return arg1))

instance (ATransition (Node16 (Node8 prev)) t) => ATransition (Node11 (Node8 prev)) t where
  a (Node11 prev arg1) = a (Node16 prev (Return arg1))

instance (ATransition (Node15 (Node9 prev)) t) => ATransition (Node11 (Node9 prev)) t where
  a (Node11 prev arg1) = a (Node15 prev (Return arg1))

instance (BTransition (Node3 (Node4 prev)) t) => BTransition (Node11 (Node4 prev)) t where
  b (Node11 prev arg1) = b (Node3 prev (Return arg1))

instance (BTransition (Node13 (Node7 prev)) t) => BTransition (Node11 (Node7 prev)) t where
  b (Node11 prev arg1) = b (Node13 prev (Return arg1))

instance (BTransition (Node16 (Node8 prev)) t) => BTransition (Node11 (Node8 prev)) t where
  b (Node11 prev arg1) = b (Node16 prev (Return arg1))

instance (BTransition (Node15 (Node9 prev)) t) => BTransition (Node11 (Node9 prev)) t where
  b (Node11 prev arg1) = b (Node15 prev (Return arg1))

instance (Else_Transition (Node3 (Node4 prev)) t) => Else_Transition (Node11 (Node4 prev)) t where
  else_ (Node11 prev arg1) = else_ (Node3 prev (Return arg1))

instance (Else_Transition (Node13 (Node7 prev)) t) => Else_Transition (Node11 (Node7 prev)) t where
  else_ (Node11 prev arg1) = else_ (Node13 prev (Return arg1))

instance (Else_Transition (Node16 (Node8 prev)) t) => Else_Transition (Node11 (Node8 prev)) t where
  else_ (Node11 prev arg1) = else_ (Node16 prev (Return arg1))

instance (Else_Transition (Node15 (Node9 prev)) t) => Else_Transition (Node11 (Node9 prev)) t where
  else_ (Node11 prev arg1) = else_ (Node15 prev (Return arg1))

instance (EndTransition (Node2 (Node1 prev)) t) => EndTransition (Node12 (Node13 (Node7 (Node1 prev)))) t where
  end (Node12 (Node13 (Node7 prev) arg1)) = end (Node2 prev (S1 arg1))

instance ATransition (Node13 prev) (Node12 (Node13 prev)) where
  a src = Node12 src

instance (EndTransition (Node2 (Node1 prev)) t) => EndTransition (Node14 (Node15 (Node9 (Node1 prev)))) t where
  end (Node14 (Node15 (Node9 prev) arg1)) = end (Node2 prev (S2 arg1))

instance BTransition (Node15 prev) (Node14 (Node15 prev)) where
  b src = Node14 src

instance (ATransition (Node5 (Node6 prev)) t) => ATransition (Node16 (Node8 (Node6 prev))) t where
  a (Node16 (Node8 prev) arg1) = a (Node5 prev (ThenClause arg1))

instance (BTransition (Node5 (Node6 prev)) t) => BTransition (Node16 (Node8 (Node6 prev))) t where
  b (Node16 (Node8 prev) arg1) = b (Node5 prev (ThenClause arg1))

instance (Else_Transition (Node5 (Node6 prev)) t) => Else_Transition (Node16 (Node8 (Node6 prev))) t where
  else_ (Node16 (Node8 prev) arg1) = else_ (Node5 prev (ThenClause arg1))

-------------------------------------------------------------------------------

begin :: Node1 ()
begin = Node1 ()

-------------------------------------------------------------------------------

