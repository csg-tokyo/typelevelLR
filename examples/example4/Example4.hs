
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Example4 where

-------------------------------------------------------------------------------

-- grammar definition

-- nameString : Name -> str
-- simpleHello : Start -> "hello"
-- helloWithName : Start -> "hello" Name

-------------------------------------------------------------------------------

type Program r a = (a -> r) -> r

program :: a -> Program r a
program a = \k -> k a

-------------------------------------------------------------------------------

-- AST nodes

data Name
  = NameString String
  deriving (Show)

data Start
  = SimpleHello
  | HelloWithName Name
  deriving (Show)

-------------------------------------------------------------------------------

-- terminal symbols

class HelloTransition s t | s -> t where
  hello :: s -> Program r t

class StrTransition s t | s -> t where
  str :: s -> String -> Program r t


-- automaton states

data S1 prev = S1 prev Start

data S2 prev = S2 prev

data S3 prev = S3 prev String

data S4 prev = S4 prev

data S5 prev = S5 prev Name

-------------------------------------------------------------------------------

-- shift transitions

instance {-# OVERLAPS #-} HelloTransition (S2 prev) (S4 (S2 prev)) where
  hello prev = program (S4 prev)


instance {-# OVERLAPS #-} StrTransition (S4 prev) (S3 (S4 prev)) where
  str prev s = program (S3 prev s)

-------------------------------------------------------------------------------

-- reduces

class Reduce s t | s -> t where
  reduce :: s -> t


instance Reduce (S5 (S4 (S2 prev))) (S1 (S2 prev)) where
  reduce (S5 (S4 prev) p1) = S1 prev (HelloWithName p1)

instance Reduce (S4 (S2 prev)) (S1 (S2 prev)) where
  reduce (S4 prev) = S1 prev (SimpleHello)

instance Reduce (S3 (S4 prev)) (S5 (S4 prev)) where
  reduce (S3 prev p1) = S5 prev (NameString p1)


-- Reduce -> Transition

instance {-# OVERLAPS #-} (Reduce r s, HelloTransition s t) =>
    HelloTransition r t where
  hello r = hello (reduce r)

instance {-# OVERLAPS #-} (Reduce r s, StrTransition s t) =>
    StrTransition r t where
  str r s = str (reduce r) s

-------------------------------------------------------------------------------

-- initial state

begin :: Program r (S2 ())
begin = program (S2 ())

-------------------------------------------------------------------------------

class End s r | s -> r where
  end :: s -> r

instance {-# OVERLAPS #-} End (S1 prev) Start where
  end (S1 _ r) = r

instance {-# OVERLAPS #-} (Reduce r s, End s t) => End r t where
  end r = end (reduce r)

-------------------------------------------------------------------------------

