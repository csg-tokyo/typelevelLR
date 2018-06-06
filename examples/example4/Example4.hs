
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Example4 where

-------------------------------------------------------------------------------

-- grammar definition

-- simpleHello : Start -> "hello()"
-- helloWithName : Start -> "hello()" Name
-- nameString : Name -> str

-------------------------------------------------------------------------------

type Program r a = (a -> r) -> r

program :: a -> Program r a
program a = \k -> k a

-------------------------------------------------------------------------------

-- AST nodes

data Start
  = SimpleHello
  | HelloWithName Name
  deriving (Show)

data Name
  = NameString Str
  deriving (Show)

data Str
  deriving (Show)

-------------------------------------------------------------------------------

-- terminal symbols

class HelloTransition s t | s -> t where
  hello :: s -> Program r t

-------------------------------------------------------------------------------

-- automaton states

data Node1 prev = Node1 prev

data Node2 prev = Node2 prev Start

data Node3 prev = Node3 prev Name

data Node4 prev = Node4 prev

data Node5 prev = Node5 prev Str

-------------------------------------------------------------------------------

-- shift transitions

instance {-# OVERLAPS #-} HelloTransition (Node1 prev) (Node4 (Node1 prev)) where
  hello prev = program (Node4 prev)

-------------------------------------------------------------------------------

-- reduces

class Reduce s t | s -> t where
  reduce :: s -> t


-- simpleHello : Start -> "hello()"

instance Reduce (Node4 (Node1 prev)) (Node2 (Node1 prev)) where
  reduce (Node4 prev) = (Node2 prev (SimpleHello))


-- helloWithName : Start -> "hello()" Name

instance Reduce (Node3 (Node4 (Node1 prev))) (Node2 (Node1 prev)) where
  reduce (Node3 (Node4 prev) x1) = (Node2 prev (HelloWithName x1))


-- nameString : Name -> str

instance Reduce (Node5 (Node4 prev)) (Node3 (Node4 prev)) where
  reduce (Node5 prev x1) = (Node3 prev (NameString x1))


-- Reduce -> Transition

instance {-# OVERLAPS #-} (Reduce r s, HelloTransition s t) => HelloTransition r t where
  hello r = hello (reduce r)

-------------------------------------------------------------------------------

-- initial state

begin :: Program r (Node1 ())
begin = program (Node1 ())

-------------------------------------------------------------------------------

class End s r | s -> r where
  end :: s -> r

instance {-# OVERLAPS #-} End (Node2 prev) Start where
  end (Node2 _ r) = r

instance {-# OVERLAPS #-} (Reduce r s, End s t) => End r t where
  end r = end (reduce r)

-------------------------------------------------------------------------------

