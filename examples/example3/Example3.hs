
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Example3 where

-------------------------------------------------------------------------------

-- grammar definition

-- ruleS0 : S -> "O()" Mod0
-- ruleS1 : S -> "I()" Mod1
-- accept : Mod0 -> eps
-- rule00 : Mod0 -> "O()" Mod0
-- rule01 : Mod0 -> "I()" Mod1
-- rule10 : Mod1 -> "O()" Mod2
-- rule11 : Mod1 -> "I()" Mod3
-- rule20 : Mod2 -> "O()" Mod4
-- rule21 : Mod2 -> "I()" Mod5
-- rule30 : Mod3 -> "O()" Mod6
-- rule31 : Mod3 -> "I()" Mod0
-- rule40 : Mod4 -> "O()" Mod1
-- rule41 : Mod4 -> "I()" Mod2
-- rule50 : Mod5 -> "O()" Mod3
-- rule51 : Mod5 -> "I()" Mod4
-- rule60 : Mod6 -> "O()" Mod5
-- rule61 : Mod6 -> "I()" Mod6

-------------------------------------------------------------------------------

type Program r a = (a -> r) -> r

program :: a -> Program r a
program a = \k -> k a

-------------------------------------------------------------------------------

-- AST nodes

data S
  = RuleS0 Mod0
  | RuleS1 Mod1
  deriving (Show)

data Mod0
  = Accept
  | Rule00 Mod0
  | Rule01 Mod1
  deriving (Show)

data Mod1
  = Rule10 Mod2
  | Rule11 Mod3
  deriving (Show)

data Mod2
  = Rule20 Mod4
  | Rule21 Mod5
  deriving (Show)

data Mod3
  = Rule30 Mod6
  | Rule31 Mod0
  deriving (Show)

data Mod4
  = Rule40 Mod1
  | Rule41 Mod2
  deriving (Show)

data Mod5
  = Rule50 Mod3
  | Rule51 Mod4
  deriving (Show)

data Mod6
  = Rule60 Mod5
  | Rule61 Mod6
  deriving (Show)

-------------------------------------------------------------------------------

-- terminal symbols

class OTransition s t | s -> t where
  o :: s -> Program r t

class ITransition s t | s -> t where
  i :: s -> Program r t

-------------------------------------------------------------------------------

-- automaton states

data Node1 prev = Node1 prev

data Node2 prev = Node2 prev S

data Node3 prev = Node3 prev

data Node4 prev = Node4 prev

data Node5 prev = Node5 prev

data Node6 prev = Node6 prev Mod0

data Node7 prev = Node7 prev Mod1

data Node8 prev = Node8 prev

data Node9 prev = Node9 prev Mod2

data Node10 prev = Node10 prev

data Node11 prev = Node11 prev

data Node12 prev = Node12 prev

data Node13 prev = Node13 prev Mod3

data Node14 prev = Node14 prev

data Node15 prev = Node15 prev Mod4

data Node16 prev = Node16 prev

data Node17 prev = Node17 prev

data Node18 prev = Node18 prev Mod5

data Node19 prev = Node19 prev

data Node20 prev = Node20 prev Mod6

data Node21 prev = Node21 prev

data Node22 prev = Node22 prev

data Node23 prev = Node23 prev Mod0

data Node24 prev = Node24 prev Mod1

data Node25 prev = Node25 prev

data Node26 prev = Node26 prev Mod2

data Node27 prev = Node27 prev Mod3

data Node28 prev = Node28 prev

data Node29 prev = Node29 prev Mod4

data Node30 prev = Node30 prev Mod5

data Node31 prev = Node31 prev

data Node32 prev = Node32 prev Mod6

data Node33 prev = Node33 prev Mod0

data Node34 prev = Node34 prev Mod1

-------------------------------------------------------------------------------

-- shift transitions

instance {-# OVERLAPS #-} ITransition (Node1 prev) (Node12 (Node1 prev)) where
  i prev = program (Node12 prev)

instance {-# OVERLAPS #-} ITransition (Node3 prev) (Node8 (Node3 prev)) where
  i prev = program (Node8 prev)

instance {-# OVERLAPS #-} ITransition (Node4 prev) (Node8 (Node4 prev)) where
  i prev = program (Node8 prev)

instance {-# OVERLAPS #-} ITransition (Node5 prev) (Node8 (Node5 prev)) where
  i prev = program (Node8 prev)

instance {-# OVERLAPS #-} ITransition (Node8 prev) (Node14 (Node8 prev)) where
  i prev = program (Node14 prev)

instance {-# OVERLAPS #-} ITransition (Node10 prev) (Node19 (Node10 prev)) where
  i prev = program (Node19 prev)

instance {-# OVERLAPS #-} ITransition (Node11 prev) (Node14 (Node11 prev)) where
  i prev = program (Node14 prev)

instance {-# OVERLAPS #-} ITransition (Node12 prev) (Node14 (Node12 prev)) where
  i prev = program (Node14 prev)

instance {-# OVERLAPS #-} ITransition (Node14 prev) (Node4 (Node14 prev)) where
  i prev = program (Node4 prev)

instance {-# OVERLAPS #-} ITransition (Node16 prev) (Node17 (Node16 prev)) where
  i prev = program (Node17 prev)

instance {-# OVERLAPS #-} ITransition (Node17 prev) (Node19 (Node17 prev)) where
  i prev = program (Node19 prev)

instance {-# OVERLAPS #-} ITransition (Node19 prev) (Node25 (Node19 prev)) where
  i prev = program (Node25 prev)

instance {-# OVERLAPS #-} ITransition (Node21 prev) (Node31 (Node21 prev)) where
  i prev = program (Node31 prev)

instance {-# OVERLAPS #-} ITransition (Node22 prev) (Node4 (Node22 prev)) where
  i prev = program (Node4 prev)

instance {-# OVERLAPS #-} ITransition (Node25 prev) (Node17 (Node25 prev)) where
  i prev = program (Node17 prev)

instance {-# OVERLAPS #-} ITransition (Node28 prev) (Node25 (Node28 prev)) where
  i prev = program (Node25 prev)

instance {-# OVERLAPS #-} ITransition (Node31 prev) (Node31 (Node31 prev)) where
  i prev = program (Node31 prev)


instance {-# OVERLAPS #-} OTransition (Node1 prev) (Node5 (Node1 prev)) where
  o prev = program (Node5 prev)

instance {-# OVERLAPS #-} OTransition (Node3 prev) (Node3 (Node3 prev)) where
  o prev = program (Node3 prev)

instance {-# OVERLAPS #-} OTransition (Node4 prev) (Node3 (Node4 prev)) where
  o prev = program (Node3 prev)

instance {-# OVERLAPS #-} OTransition (Node5 prev) (Node3 (Node5 prev)) where
  o prev = program (Node3 prev)

instance {-# OVERLAPS #-} OTransition (Node8 prev) (Node10 (Node8 prev)) where
  o prev = program (Node10 prev)

instance {-# OVERLAPS #-} OTransition (Node10 prev) (Node16 (Node10 prev)) where
  o prev = program (Node16 prev)

instance {-# OVERLAPS #-} OTransition (Node11 prev) (Node10 (Node11 prev)) where
  o prev = program (Node10 prev)

instance {-# OVERLAPS #-} OTransition (Node12 prev) (Node10 (Node12 prev)) where
  o prev = program (Node10 prev)

instance {-# OVERLAPS #-} OTransition (Node14 prev) (Node21 (Node14 prev)) where
  o prev = program (Node21 prev)

instance {-# OVERLAPS #-} OTransition (Node16 prev) (Node11 (Node16 prev)) where
  o prev = program (Node11 prev)

instance {-# OVERLAPS #-} OTransition (Node17 prev) (Node16 (Node17 prev)) where
  o prev = program (Node16 prev)

instance {-# OVERLAPS #-} OTransition (Node19 prev) (Node22 (Node19 prev)) where
  o prev = program (Node22 prev)

instance {-# OVERLAPS #-} OTransition (Node21 prev) (Node28 (Node21 prev)) where
  o prev = program (Node28 prev)

instance {-# OVERLAPS #-} OTransition (Node22 prev) (Node21 (Node22 prev)) where
  o prev = program (Node21 prev)

instance {-# OVERLAPS #-} OTransition (Node25 prev) (Node11 (Node25 prev)) where
  o prev = program (Node11 prev)

instance {-# OVERLAPS #-} OTransition (Node28 prev) (Node22 (Node28 prev)) where
  o prev = program (Node22 prev)

instance {-# OVERLAPS #-} OTransition (Node31 prev) (Node28 (Node31 prev)) where
  o prev = program (Node28 prev)

-------------------------------------------------------------------------------

-- reduces

class Reduce s t | s -> t where
  reduce :: s -> t


-- ruleS0 : S -> "O()" Mod0

instance Reduce (Node33 (Node5 (Node1 prev))) (Node2 (Node1 prev)) where
  reduce (Node33 (Node5 prev) x1) = (Node2 prev (RuleS0 x1))


-- ruleS1 : S -> "I()" Mod1

instance Reduce (Node34 (Node12 (Node1 prev))) (Node2 (Node1 prev)) where
  reduce (Node34 (Node12 prev) x1) = (Node2 prev (RuleS1 x1))


-- accept : Mod0 -> eps

instance Reduce (Node3 prev) (Node6 (Node3 prev)) where
  reduce prev = (Node6 prev (Accept))

instance Reduce (Node4 prev) (Node23 (Node4 prev)) where
  reduce prev = (Node23 prev (Accept))

instance Reduce (Node5 prev) (Node33 (Node5 prev)) where
  reduce prev = (Node33 prev (Accept))


-- rule00 : Mod0 -> "O()" Mod0

instance Reduce (Node6 (Node3 (Node3 prev))) (Node6 (Node3 prev)) where
  reduce (Node6 (Node3 prev) x1) = (Node6 prev (Rule00 x1))

instance Reduce (Node6 (Node3 (Node4 prev))) (Node23 (Node4 prev)) where
  reduce (Node6 (Node3 prev) x1) = (Node23 prev (Rule00 x1))

instance Reduce (Node6 (Node3 (Node5 prev))) (Node33 (Node5 prev)) where
  reduce (Node6 (Node3 prev) x1) = (Node33 prev (Rule00 x1))


-- rule01 : Mod0 -> "I()" Mod1

instance Reduce (Node7 (Node8 (Node3 prev))) (Node6 (Node3 prev)) where
  reduce (Node7 (Node8 prev) x1) = (Node6 prev (Rule01 x1))

instance Reduce (Node7 (Node8 (Node4 prev))) (Node23 (Node4 prev)) where
  reduce (Node7 (Node8 prev) x1) = (Node23 prev (Rule01 x1))

instance Reduce (Node7 (Node8 (Node5 prev))) (Node33 (Node5 prev)) where
  reduce (Node7 (Node8 prev) x1) = (Node33 prev (Rule01 x1))


-- rule10 : Mod1 -> "O()" Mod2

instance Reduce (Node9 (Node10 (Node8 prev))) (Node7 (Node8 prev)) where
  reduce (Node9 (Node10 prev) x1) = (Node7 prev (Rule10 x1))

instance Reduce (Node9 (Node10 (Node11 prev))) (Node24 (Node11 prev)) where
  reduce (Node9 (Node10 prev) x1) = (Node24 prev (Rule10 x1))

instance Reduce (Node9 (Node10 (Node12 prev))) (Node34 (Node12 prev)) where
  reduce (Node9 (Node10 prev) x1) = (Node34 prev (Rule10 x1))


-- rule11 : Mod1 -> "I()" Mod3

instance Reduce (Node13 (Node14 (Node8 prev))) (Node7 (Node8 prev)) where
  reduce (Node13 (Node14 prev) x1) = (Node7 prev (Rule11 x1))

instance Reduce (Node13 (Node14 (Node11 prev))) (Node24 (Node11 prev)) where
  reduce (Node13 (Node14 prev) x1) = (Node24 prev (Rule11 x1))

instance Reduce (Node13 (Node14 (Node12 prev))) (Node34 (Node12 prev)) where
  reduce (Node13 (Node14 prev) x1) = (Node34 prev (Rule11 x1))


-- rule20 : Mod2 -> "O()" Mod4

instance Reduce (Node15 (Node16 (Node10 prev))) (Node9 (Node10 prev)) where
  reduce (Node15 (Node16 prev) x1) = (Node9 prev (Rule20 x1))

instance Reduce (Node15 (Node16 (Node17 prev))) (Node26 (Node17 prev)) where
  reduce (Node15 (Node16 prev) x1) = (Node26 prev (Rule20 x1))


-- rule21 : Mod2 -> "I()" Mod5

instance Reduce (Node18 (Node19 (Node10 prev))) (Node9 (Node10 prev)) where
  reduce (Node18 (Node19 prev) x1) = (Node9 prev (Rule21 x1))

instance Reduce (Node18 (Node19 (Node17 prev))) (Node26 (Node17 prev)) where
  reduce (Node18 (Node19 prev) x1) = (Node26 prev (Rule21 x1))


-- rule30 : Mod3 -> "O()" Mod6

instance Reduce (Node20 (Node21 (Node14 prev))) (Node13 (Node14 prev)) where
  reduce (Node20 (Node21 prev) x1) = (Node13 prev (Rule30 x1))

instance Reduce (Node20 (Node21 (Node22 prev))) (Node27 (Node22 prev)) where
  reduce (Node20 (Node21 prev) x1) = (Node27 prev (Rule30 x1))


-- rule31 : Mod3 -> "I()" Mod0

instance Reduce (Node23 (Node4 (Node14 prev))) (Node13 (Node14 prev)) where
  reduce (Node23 (Node4 prev) x1) = (Node13 prev (Rule31 x1))

instance Reduce (Node23 (Node4 (Node22 prev))) (Node27 (Node22 prev)) where
  reduce (Node23 (Node4 prev) x1) = (Node27 prev (Rule31 x1))


-- rule40 : Mod4 -> "O()" Mod1

instance Reduce (Node24 (Node11 (Node16 prev))) (Node15 (Node16 prev)) where
  reduce (Node24 (Node11 prev) x1) = (Node15 prev (Rule40 x1))

instance Reduce (Node24 (Node11 (Node25 prev))) (Node29 (Node25 prev)) where
  reduce (Node24 (Node11 prev) x1) = (Node29 prev (Rule40 x1))


-- rule41 : Mod4 -> "I()" Mod2

instance Reduce (Node26 (Node17 (Node16 prev))) (Node15 (Node16 prev)) where
  reduce (Node26 (Node17 prev) x1) = (Node15 prev (Rule41 x1))

instance Reduce (Node26 (Node17 (Node25 prev))) (Node29 (Node25 prev)) where
  reduce (Node26 (Node17 prev) x1) = (Node29 prev (Rule41 x1))


-- rule50 : Mod5 -> "O()" Mod3

instance Reduce (Node27 (Node22 (Node19 prev))) (Node18 (Node19 prev)) where
  reduce (Node27 (Node22 prev) x1) = (Node18 prev (Rule50 x1))

instance Reduce (Node27 (Node22 (Node28 prev))) (Node30 (Node28 prev)) where
  reduce (Node27 (Node22 prev) x1) = (Node30 prev (Rule50 x1))


-- rule51 : Mod5 -> "I()" Mod4

instance Reduce (Node29 (Node25 (Node19 prev))) (Node18 (Node19 prev)) where
  reduce (Node29 (Node25 prev) x1) = (Node18 prev (Rule51 x1))

instance Reduce (Node29 (Node25 (Node28 prev))) (Node30 (Node28 prev)) where
  reduce (Node29 (Node25 prev) x1) = (Node30 prev (Rule51 x1))


-- rule60 : Mod6 -> "O()" Mod5

instance Reduce (Node30 (Node28 (Node21 prev))) (Node20 (Node21 prev)) where
  reduce (Node30 (Node28 prev) x1) = (Node20 prev (Rule60 x1))

instance Reduce (Node30 (Node28 (Node31 prev))) (Node32 (Node31 prev)) where
  reduce (Node30 (Node28 prev) x1) = (Node32 prev (Rule60 x1))


-- rule61 : Mod6 -> "I()" Mod6

instance Reduce (Node32 (Node31 (Node21 prev))) (Node20 (Node21 prev)) where
  reduce (Node32 (Node31 prev) x1) = (Node20 prev (Rule61 x1))

instance Reduce (Node32 (Node31 (Node31 prev))) (Node32 (Node31 prev)) where
  reduce (Node32 (Node31 prev) x1) = (Node32 prev (Rule61 x1))


-- Reduce -> Transition

instance {-# OVERLAPS #-} (Reduce r s, OTransition s t) => OTransition r t where
  o r = o (reduce r)

instance {-# OVERLAPS #-} (Reduce r s, ITransition s t) => ITransition r t where
  i r = i (reduce r)

-------------------------------------------------------------------------------

-- initial state

begin :: Program r (Node1 ())
begin = program (Node1 ())

-------------------------------------------------------------------------------

class End s r | s -> r where
  end :: s -> r

instance {-# OVERLAPS #-} End (Node2 prev) S where
  end (Node2 _ r) = r

instance {-# OVERLAPS #-} (Reduce r s, End s t) => End r t where
  end r = end (reduce r)

-------------------------------------------------------------------------------

