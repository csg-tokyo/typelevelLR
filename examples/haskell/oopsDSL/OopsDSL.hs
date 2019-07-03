
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module OopsDSL where

-------------------------------------------------------------------------------

-- grammar definition

-- AddPs : Oops -> Os "ps()"
-- AddO : Os -> "o()" Os
-- EndOs : Os -> eps

-------------------------------------------------------------------------------

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

-------------------------------------------------------------------------------

-- AST nodes

data Oops
  = AddPs Os
  deriving (Show)

data Os
  = AddO Os
  | EndOs
  deriving (Show)

-------------------------------------------------------------------------------

-- terminal symbols

class PsTransition s t | s -> t where
  ps :: s -> t

class OTransition s t | s -> t where
  o :: s -> t

class EndTransition s t | s -> t where
  end :: s -> t

-------------------------------------------------------------------------------

-- automaton states

data Node1 prev = Node1 prev

data Node2 prev = Node2 prev Oops

data Node3 prev = Node3 prev Os

data Node4 prev = Node4 prev

data Node5 prev = Node5 prev

data Node6 prev = Node6 prev Os

-------------------------------------------------------------------------------

-- transition instances

instance EndTransition (Node2 prev) Oops where
  end (Node2 _ arg1) = arg1

instance OTransition (Node1 prev) (Node4 (Node1 prev)) where
  o src = Node4 src

instance (PsTransition (Node6 (Node1 prev)) t) => PsTransition (Node1 prev) t where
  ps prev = ps (Node6 prev (EndOs))

instance (PsTransition (Node6 (Node1 prev)) t) => PsTransition (Node3 (Node4 (Node1 prev))) t where
  ps (Node3 (Node4 prev) arg1) = ps (Node6 prev (AddO arg1))

instance (PsTransition (Node3 (Node4 prev)) t) => PsTransition (Node3 (Node4 (Node4 prev))) t where
  ps (Node3 (Node4 prev) arg1) = ps (Node3 prev (AddO arg1))

instance OTransition (Node4 prev) (Node4 (Node4 prev)) where
  o src = Node4 src

instance (PsTransition (Node3 (Node4 prev)) t) => PsTransition (Node4 prev) t where
  ps prev = ps (Node3 prev (EndOs))

instance (EndTransition (Node2 (Node1 prev)) t) => EndTransition (Node5 (Node6 (Node1 prev))) t where
  end (Node5 (Node6 prev arg1)) = end (Node2 prev (AddPs arg1))

instance PsTransition (Node6 prev) (Node5 (Node6 prev)) where
  ps src = Node5 src

-------------------------------------------------------------------------------

begin :: Node1 ()
begin = Node1 ()

-------------------------------------------------------------------------------

