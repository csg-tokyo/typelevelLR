
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Example1 where

-------------------------------------------------------------------------------

-- grammar definition

-- genA : A -> "a()" A
-- BToA : A -> B
-- genAB : B -> "a()" B "b()"
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

-------------------------------------------------------------------------------

-- automaton states

data Node1 prev = Node1 prev

data Node2 prev = Node2 prev A

data Node3 prev = Node3 prev B

data Node4 prev = Node4 prev B

data Node5 prev = Node5 prev

data Node6 prev = Node6 prev A

data Node7 prev = Node7 prev

-------------------------------------------------------------------------------

-- shift transitions

instance {-# OVERLAPS #-} ATransition (Node1 prev) (Node5 (Node1 prev)) where
  a prev = program (Node5 prev)

instance {-# OVERLAPS #-} ATransition (Node5 prev) (Node5 (Node5 prev)) where
  a prev = program (Node5 prev)


instance {-# OVERLAPS #-} BTransition (Node4 prev) (Node7 (Node4 prev)) where
  b prev = program (Node7 prev)

-------------------------------------------------------------------------------

-- reduces

class Reduce s t | s -> t where
  reduce :: s -> t


-- genA : A -> "a()" A

instance Reduce (Node6 (Node5 (Node1 prev))) (Node2 (Node1 prev)) where
  reduce (Node6 (Node5 prev) x1) = (Node2 prev (GenA x1))

instance Reduce (Node6 (Node5 (Node5 prev))) (Node6 (Node5 prev)) where
  reduce (Node6 (Node5 prev) x1) = (Node6 prev (GenA x1))


-- BToA : A -> B

instance Reduce (Node3 (Node1 prev)) (Node2 (Node1 prev)) where
  reduce (Node3 prev x1) = (Node2 prev (BToA x1))

instance Reduce (Node4 (Node5 prev)) (Node6 (Node5 prev)) where
  reduce (Node4 prev x1) = (Node6 prev (BToA x1))


-- genAB : B -> "a()" B "b()"

instance Reduce (Node7 (Node4 (Node5 (Node1 prev)))) (Node3 (Node1 prev)) where
  reduce (Node7 (Node4 (Node5 prev) x1)) = (Node3 prev (GenAB x1))

instance Reduce (Node7 (Node4 (Node5 (Node5 prev)))) (Node4 (Node5 prev)) where
  reduce (Node7 (Node4 (Node5 prev) x1)) = (Node4 prev (GenAB x1))


-- fin : B -> eps

instance Reduce (Node1 prev) (Node3 (Node1 prev)) where
  reduce prev = (Node3 prev (Fin))

instance Reduce (Node5 prev) (Node4 (Node5 prev)) where
  reduce prev = (Node4 prev (Fin))


-- Reduce -> Transition

instance {-# OVERLAPS #-} (Reduce r s, ATransition s t) => ATransition r t where
  a r = a (reduce r)

instance {-# OVERLAPS #-} (Reduce r s, BTransition s t) => BTransition r t where
  b r = b (reduce r)

-------------------------------------------------------------------------------

-- initial state

begin :: Program r (Node1 ())
begin = program (Node1 ())

-------------------------------------------------------------------------------

class End s r | s -> r where
  end :: s -> r

instance {-# OVERLAPS #-} End (Node2 prev) A where
  end (Node2 _ r) = r

instance {-# OVERLAPS #-} (Reduce r s, End s t) => End r t where
  end r = end (reduce r)

-------------------------------------------------------------------------------

