
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MultipleOf7 where

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

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

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
  o :: s -> t

class ITransition s t | s -> t where
  i :: s -> t

class EndTransition s t | s -> t where
  end :: s -> t

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

-- transition instances

instance EndTransition (Node2 prev) S where
  end (Node2 _ arg1) = arg1

instance ITransition (Node1 prev) (Node12 (Node1 prev)) where
  i src = Node12 src

instance OTransition (Node1 prev) (Node5 (Node1 prev)) where
  o src = Node5 src

instance ITransition (Node3 prev) (Node8 (Node3 prev)) where
  i src = Node8 src

instance OTransition (Node3 prev) (Node3 (Node3 prev)) where
  o src = Node3 src

instance (EndTransition (Node6 (Node3 prev)) t) => EndTransition (Node3 prev) t where
  end prev = end (Node6 prev (Accept))

instance ITransition (Node4 prev) (Node8 (Node4 prev)) where
  i src = Node8 src

instance OTransition (Node4 prev) (Node3 (Node4 prev)) where
  o src = Node3 src

instance (EndTransition (Node23 (Node4 prev)) t) => EndTransition (Node4 prev) t where
  end prev = end (Node23 prev (Accept))

instance ITransition (Node5 prev) (Node8 (Node5 prev)) where
  i src = Node8 src

instance OTransition (Node5 prev) (Node3 (Node5 prev)) where
  o src = Node3 src

instance (EndTransition (Node33 (Node5 prev)) t) => EndTransition (Node5 prev) t where
  end prev = end (Node33 prev (Accept))

instance (EndTransition (Node6 (Node3 prev)) t) => EndTransition (Node6 (Node3 (Node3 prev))) t where
  end (Node6 (Node3 prev) arg1) = end (Node6 prev (Rule00 arg1))

instance (EndTransition (Node23 (Node4 prev)) t) => EndTransition (Node6 (Node3 (Node4 prev))) t where
  end (Node6 (Node3 prev) arg1) = end (Node23 prev (Rule00 arg1))

instance (EndTransition (Node33 (Node5 prev)) t) => EndTransition (Node6 (Node3 (Node5 prev))) t where
  end (Node6 (Node3 prev) arg1) = end (Node33 prev (Rule00 arg1))

instance (EndTransition (Node6 (Node3 prev)) t) => EndTransition (Node7 (Node8 (Node3 prev))) t where
  end (Node7 (Node8 prev) arg1) = end (Node6 prev (Rule01 arg1))

instance (EndTransition (Node23 (Node4 prev)) t) => EndTransition (Node7 (Node8 (Node4 prev))) t where
  end (Node7 (Node8 prev) arg1) = end (Node23 prev (Rule01 arg1))

instance (EndTransition (Node33 (Node5 prev)) t) => EndTransition (Node7 (Node8 (Node5 prev))) t where
  end (Node7 (Node8 prev) arg1) = end (Node33 prev (Rule01 arg1))

instance ITransition (Node8 prev) (Node14 (Node8 prev)) where
  i src = Node14 src

instance OTransition (Node8 prev) (Node10 (Node8 prev)) where
  o src = Node10 src

instance (EndTransition (Node7 (Node8 prev)) t) => EndTransition (Node9 (Node10 (Node8 prev))) t where
  end (Node9 (Node10 prev) arg1) = end (Node7 prev (Rule10 arg1))

instance (EndTransition (Node24 (Node11 prev)) t) => EndTransition (Node9 (Node10 (Node11 prev))) t where
  end (Node9 (Node10 prev) arg1) = end (Node24 prev (Rule10 arg1))

instance (EndTransition (Node34 (Node12 prev)) t) => EndTransition (Node9 (Node10 (Node12 prev))) t where
  end (Node9 (Node10 prev) arg1) = end (Node34 prev (Rule10 arg1))

instance ITransition (Node10 prev) (Node19 (Node10 prev)) where
  i src = Node19 src

instance OTransition (Node10 prev) (Node16 (Node10 prev)) where
  o src = Node16 src

instance ITransition (Node11 prev) (Node14 (Node11 prev)) where
  i src = Node14 src

instance OTransition (Node11 prev) (Node10 (Node11 prev)) where
  o src = Node10 src

instance ITransition (Node12 prev) (Node14 (Node12 prev)) where
  i src = Node14 src

instance OTransition (Node12 prev) (Node10 (Node12 prev)) where
  o src = Node10 src

instance (EndTransition (Node7 (Node8 prev)) t) => EndTransition (Node13 (Node14 (Node8 prev))) t where
  end (Node13 (Node14 prev) arg1) = end (Node7 prev (Rule11 arg1))

instance (EndTransition (Node24 (Node11 prev)) t) => EndTransition (Node13 (Node14 (Node11 prev))) t where
  end (Node13 (Node14 prev) arg1) = end (Node24 prev (Rule11 arg1))

instance (EndTransition (Node34 (Node12 prev)) t) => EndTransition (Node13 (Node14 (Node12 prev))) t where
  end (Node13 (Node14 prev) arg1) = end (Node34 prev (Rule11 arg1))

instance ITransition (Node14 prev) (Node4 (Node14 prev)) where
  i src = Node4 src

instance OTransition (Node14 prev) (Node21 (Node14 prev)) where
  o src = Node21 src

instance (EndTransition (Node9 (Node10 prev)) t) => EndTransition (Node15 (Node16 (Node10 prev))) t where
  end (Node15 (Node16 prev) arg1) = end (Node9 prev (Rule20 arg1))

instance (EndTransition (Node26 (Node17 prev)) t) => EndTransition (Node15 (Node16 (Node17 prev))) t where
  end (Node15 (Node16 prev) arg1) = end (Node26 prev (Rule20 arg1))

instance ITransition (Node16 prev) (Node17 (Node16 prev)) where
  i src = Node17 src

instance OTransition (Node16 prev) (Node11 (Node16 prev)) where
  o src = Node11 src

instance ITransition (Node17 prev) (Node19 (Node17 prev)) where
  i src = Node19 src

instance OTransition (Node17 prev) (Node16 (Node17 prev)) where
  o src = Node16 src

instance (EndTransition (Node9 (Node10 prev)) t) => EndTransition (Node18 (Node19 (Node10 prev))) t where
  end (Node18 (Node19 prev) arg1) = end (Node9 prev (Rule21 arg1))

instance (EndTransition (Node26 (Node17 prev)) t) => EndTransition (Node18 (Node19 (Node17 prev))) t where
  end (Node18 (Node19 prev) arg1) = end (Node26 prev (Rule21 arg1))

instance ITransition (Node19 prev) (Node25 (Node19 prev)) where
  i src = Node25 src

instance OTransition (Node19 prev) (Node22 (Node19 prev)) where
  o src = Node22 src

instance (EndTransition (Node13 (Node14 prev)) t) => EndTransition (Node20 (Node21 (Node14 prev))) t where
  end (Node20 (Node21 prev) arg1) = end (Node13 prev (Rule30 arg1))

instance (EndTransition (Node27 (Node22 prev)) t) => EndTransition (Node20 (Node21 (Node22 prev))) t where
  end (Node20 (Node21 prev) arg1) = end (Node27 prev (Rule30 arg1))

instance ITransition (Node21 prev) (Node31 (Node21 prev)) where
  i src = Node31 src

instance OTransition (Node21 prev) (Node28 (Node21 prev)) where
  o src = Node28 src

instance ITransition (Node22 prev) (Node4 (Node22 prev)) where
  i src = Node4 src

instance OTransition (Node22 prev) (Node21 (Node22 prev)) where
  o src = Node21 src

instance (EndTransition (Node13 (Node14 prev)) t) => EndTransition (Node23 (Node4 (Node14 prev))) t where
  end (Node23 (Node4 prev) arg1) = end (Node13 prev (Rule31 arg1))

instance (EndTransition (Node27 (Node22 prev)) t) => EndTransition (Node23 (Node4 (Node22 prev))) t where
  end (Node23 (Node4 prev) arg1) = end (Node27 prev (Rule31 arg1))

instance (EndTransition (Node15 (Node16 prev)) t) => EndTransition (Node24 (Node11 (Node16 prev))) t where
  end (Node24 (Node11 prev) arg1) = end (Node15 prev (Rule40 arg1))

instance (EndTransition (Node29 (Node25 prev)) t) => EndTransition (Node24 (Node11 (Node25 prev))) t where
  end (Node24 (Node11 prev) arg1) = end (Node29 prev (Rule40 arg1))

instance ITransition (Node25 prev) (Node17 (Node25 prev)) where
  i src = Node17 src

instance OTransition (Node25 prev) (Node11 (Node25 prev)) where
  o src = Node11 src

instance (EndTransition (Node15 (Node16 prev)) t) => EndTransition (Node26 (Node17 (Node16 prev))) t where
  end (Node26 (Node17 prev) arg1) = end (Node15 prev (Rule41 arg1))

instance (EndTransition (Node29 (Node25 prev)) t) => EndTransition (Node26 (Node17 (Node25 prev))) t where
  end (Node26 (Node17 prev) arg1) = end (Node29 prev (Rule41 arg1))

instance (EndTransition (Node18 (Node19 prev)) t) => EndTransition (Node27 (Node22 (Node19 prev))) t where
  end (Node27 (Node22 prev) arg1) = end (Node18 prev (Rule50 arg1))

instance (EndTransition (Node30 (Node28 prev)) t) => EndTransition (Node27 (Node22 (Node28 prev))) t where
  end (Node27 (Node22 prev) arg1) = end (Node30 prev (Rule50 arg1))

instance ITransition (Node28 prev) (Node25 (Node28 prev)) where
  i src = Node25 src

instance OTransition (Node28 prev) (Node22 (Node28 prev)) where
  o src = Node22 src

instance (EndTransition (Node18 (Node19 prev)) t) => EndTransition (Node29 (Node25 (Node19 prev))) t where
  end (Node29 (Node25 prev) arg1) = end (Node18 prev (Rule51 arg1))

instance (EndTransition (Node30 (Node28 prev)) t) => EndTransition (Node29 (Node25 (Node28 prev))) t where
  end (Node29 (Node25 prev) arg1) = end (Node30 prev (Rule51 arg1))

instance (EndTransition (Node20 (Node21 prev)) t) => EndTransition (Node30 (Node28 (Node21 prev))) t where
  end (Node30 (Node28 prev) arg1) = end (Node20 prev (Rule60 arg1))

instance (EndTransition (Node32 (Node31 prev)) t) => EndTransition (Node30 (Node28 (Node31 prev))) t where
  end (Node30 (Node28 prev) arg1) = end (Node32 prev (Rule60 arg1))

instance ITransition (Node31 prev) (Node31 (Node31 prev)) where
  i src = Node31 src

instance OTransition (Node31 prev) (Node28 (Node31 prev)) where
  o src = Node28 src

instance (EndTransition (Node20 (Node21 prev)) t) => EndTransition (Node32 (Node31 (Node21 prev))) t where
  end (Node32 (Node31 prev) arg1) = end (Node20 prev (Rule61 arg1))

instance (EndTransition (Node32 (Node31 prev)) t) => EndTransition (Node32 (Node31 (Node31 prev))) t where
  end (Node32 (Node31 prev) arg1) = end (Node32 prev (Rule61 arg1))

instance (EndTransition (Node2 (Node1 prev)) t) => EndTransition (Node33 (Node5 (Node1 prev))) t where
  end (Node33 (Node5 prev) arg1) = end (Node2 prev (RuleS0 arg1))

instance (EndTransition (Node2 (Node1 prev)) t) => EndTransition (Node34 (Node12 (Node1 prev))) t where
  end (Node34 (Node12 prev) arg1) = end (Node2 prev (RuleS1 arg1))

-------------------------------------------------------------------------------

begin :: Node1 ()
begin = Node1 ()

-------------------------------------------------------------------------------

