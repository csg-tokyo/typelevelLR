
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Example3 where

-------------------------------------------------------------------------------

-- grammar definition

-- accept : Mod0 -> eps
-- rule00 : Mod0 -> "O" Mod0
-- rule01 : Mod0 -> "I" Mod1
-- rule10 : Mod1 -> "O" Mod2
-- rule11 : Mod1 -> "I" Mod3
-- rule20 : Mod2 -> "O" Mod4
-- rule21 : Mod2 -> "I" Mod5
-- rule30 : Mod3 -> "O" Mod6
-- rule31 : Mod3 -> "I" Mod0
-- rule40 : Mod4 -> "O" Mod1
-- rule41 : Mod4 -> "I" Mod2
-- rule50 : Mod5 -> "O" Mod3
-- rule51 : Mod5 -> "I" Mod4
-- rule60 : Mod6 -> "O" Mod5
-- rule61 : Mod6 -> "I" Mod6
-- ruleS0 : S -> "O" Mod0
-- ruleS1 : S -> "I" Mod1

-------------------------------------------------------------------------------

type Program r a = (a -> r) -> r

program :: a -> Program r a
program a = \k -> k a

-------------------------------------------------------------------------------

-- AST nodes

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

data S
  = RuleS0 Mod0
  | RuleS1 Mod1
  deriving (Show)

-------------------------------------------------------------------------------

-- terminal symbols

class ITransition s t | s -> t where
  i :: s -> Program r t

class OTransition s t | s -> t where
  o :: s -> Program r t


-- automaton states

data S1 prev = S1 prev S

data S2 prev = S2 prev

data S3 prev = S3 prev

data S4 prev = S4 prev

data S5 prev = S5 prev

data S6 prev = S6 prev Mod0

data S7 prev = S7 prev Mod1

data S8 prev = S8 prev

data S9 prev = S9 prev Mod2

data S10 prev = S10 prev

data S11 prev = S11 prev

data S12 prev = S12 prev

data S13 prev = S13 prev Mod3

data S14 prev = S14 prev

data S15 prev = S15 prev Mod4

data S16 prev = S16 prev

data S17 prev = S17 prev

data S18 prev = S18 prev Mod5

data S19 prev = S19 prev

data S20 prev = S20 prev Mod6

data S21 prev = S21 prev

data S22 prev = S22 prev

data S23 prev = S23 prev Mod0

data S24 prev = S24 prev Mod1

data S25 prev = S25 prev

data S26 prev = S26 prev Mod2

data S27 prev = S27 prev Mod3

data S28 prev = S28 prev

data S29 prev = S29 prev Mod4

data S30 prev = S30 prev Mod5

data S31 prev = S31 prev

data S32 prev = S32 prev Mod6

data S33 prev = S33 prev Mod0

data S34 prev = S34 prev Mod1

-------------------------------------------------------------------------------

-- shift transitions

instance {-# OVERLAPS #-} ITransition (S2 prev) (S12 (S2 prev)) where
  i prev = program (S12 prev)

instance {-# OVERLAPS #-} OTransition (S2 prev) (S5 (S2 prev)) where
  o prev = program (S5 prev)


instance {-# OVERLAPS #-} ITransition (S3 prev) (S8 (S3 prev)) where
  i prev = program (S8 prev)

instance {-# OVERLAPS #-} OTransition (S3 prev) (S3 (S3 prev)) where
  o prev = program (S3 prev)


instance {-# OVERLAPS #-} ITransition (S4 prev) (S8 (S4 prev)) where
  i prev = program (S8 prev)

instance {-# OVERLAPS #-} OTransition (S4 prev) (S3 (S4 prev)) where
  o prev = program (S3 prev)


instance {-# OVERLAPS #-} ITransition (S5 prev) (S8 (S5 prev)) where
  i prev = program (S8 prev)

instance {-# OVERLAPS #-} OTransition (S5 prev) (S3 (S5 prev)) where
  o prev = program (S3 prev)


instance {-# OVERLAPS #-} ITransition (S8 prev) (S14 (S8 prev)) where
  i prev = program (S14 prev)

instance {-# OVERLAPS #-} OTransition (S8 prev) (S10 (S8 prev)) where
  o prev = program (S10 prev)


instance {-# OVERLAPS #-} ITransition (S10 prev) (S19 (S10 prev)) where
  i prev = program (S19 prev)

instance {-# OVERLAPS #-} OTransition (S10 prev) (S16 (S10 prev)) where
  o prev = program (S16 prev)


instance {-# OVERLAPS #-} ITransition (S11 prev) (S14 (S11 prev)) where
  i prev = program (S14 prev)

instance {-# OVERLAPS #-} OTransition (S11 prev) (S10 (S11 prev)) where
  o prev = program (S10 prev)


instance {-# OVERLAPS #-} ITransition (S12 prev) (S14 (S12 prev)) where
  i prev = program (S14 prev)

instance {-# OVERLAPS #-} OTransition (S12 prev) (S10 (S12 prev)) where
  o prev = program (S10 prev)


instance {-# OVERLAPS #-} ITransition (S14 prev) (S4 (S14 prev)) where
  i prev = program (S4 prev)

instance {-# OVERLAPS #-} OTransition (S14 prev) (S21 (S14 prev)) where
  o prev = program (S21 prev)


instance {-# OVERLAPS #-} ITransition (S16 prev) (S17 (S16 prev)) where
  i prev = program (S17 prev)

instance {-# OVERLAPS #-} OTransition (S16 prev) (S11 (S16 prev)) where
  o prev = program (S11 prev)


instance {-# OVERLAPS #-} ITransition (S17 prev) (S19 (S17 prev)) where
  i prev = program (S19 prev)

instance {-# OVERLAPS #-} OTransition (S17 prev) (S16 (S17 prev)) where
  o prev = program (S16 prev)


instance {-# OVERLAPS #-} ITransition (S19 prev) (S25 (S19 prev)) where
  i prev = program (S25 prev)

instance {-# OVERLAPS #-} OTransition (S19 prev) (S22 (S19 prev)) where
  o prev = program (S22 prev)


instance {-# OVERLAPS #-} ITransition (S21 prev) (S31 (S21 prev)) where
  i prev = program (S31 prev)

instance {-# OVERLAPS #-} OTransition (S21 prev) (S28 (S21 prev)) where
  o prev = program (S28 prev)


instance {-# OVERLAPS #-} ITransition (S22 prev) (S4 (S22 prev)) where
  i prev = program (S4 prev)

instance {-# OVERLAPS #-} OTransition (S22 prev) (S21 (S22 prev)) where
  o prev = program (S21 prev)


instance {-# OVERLAPS #-} ITransition (S25 prev) (S17 (S25 prev)) where
  i prev = program (S17 prev)

instance {-# OVERLAPS #-} OTransition (S25 prev) (S11 (S25 prev)) where
  o prev = program (S11 prev)


instance {-# OVERLAPS #-} ITransition (S28 prev) (S25 (S28 prev)) where
  i prev = program (S25 prev)

instance {-# OVERLAPS #-} OTransition (S28 prev) (S22 (S28 prev)) where
  o prev = program (S22 prev)


instance {-# OVERLAPS #-} ITransition (S31 prev) (S31 (S31 prev)) where
  i prev = program (S31 prev)

instance {-# OVERLAPS #-} OTransition (S31 prev) (S28 (S31 prev)) where
  o prev = program (S28 prev)

-------------------------------------------------------------------------------

-- reduces

class Reduce s t | s -> t where
  reduce :: s -> t


instance Reduce (S33 (S5 (S2 prev))) (S1 (S2 prev)) where
  reduce (S33 (S5 prev) p1) = S1 prev (RuleS0 p1)

instance Reduce (S34 (S12 (S2 prev))) (S1 (S2 prev)) where
  reduce (S34 (S12 prev) p1) = S1 prev (RuleS1 p1)

instance Reduce (S3 prev) (S6 (S3 prev)) where
  reduce prev = S6 prev (Accept)

instance Reduce (S6 (S3 (S3 prev))) (S6 (S3 prev)) where
  reduce (S6 (S3 prev) p1) = S6 prev (Rule00 p1)

instance Reduce (S7 (S8 (S3 prev))) (S6 (S3 prev)) where
  reduce (S7 (S8 prev) p1) = S6 prev (Rule01 p1)

instance Reduce (S4 prev) (S23 (S4 prev)) where
  reduce prev = S23 prev (Accept)

instance Reduce (S6 (S3 (S4 prev))) (S23 (S4 prev)) where
  reduce (S6 (S3 prev) p1) = S23 prev (Rule00 p1)

instance Reduce (S7 (S8 (S4 prev))) (S23 (S4 prev)) where
  reduce (S7 (S8 prev) p1) = S23 prev (Rule01 p1)

instance Reduce (S5 prev) (S33 (S5 prev)) where
  reduce prev = S33 prev (Accept)

instance Reduce (S6 (S3 (S5 prev))) (S33 (S5 prev)) where
  reduce (S6 (S3 prev) p1) = S33 prev (Rule00 p1)

instance Reduce (S7 (S8 (S5 prev))) (S33 (S5 prev)) where
  reduce (S7 (S8 prev) p1) = S33 prev (Rule01 p1)

instance Reduce (S9 (S10 (S8 prev))) (S7 (S8 prev)) where
  reduce (S9 (S10 prev) p1) = S7 prev (Rule10 p1)

instance Reduce (S13 (S14 (S8 prev))) (S7 (S8 prev)) where
  reduce (S13 (S14 prev) p1) = S7 prev (Rule11 p1)

instance Reduce (S15 (S16 (S10 prev))) (S9 (S10 prev)) where
  reduce (S15 (S16 prev) p1) = S9 prev (Rule20 p1)

instance Reduce (S18 (S19 (S10 prev))) (S9 (S10 prev)) where
  reduce (S18 (S19 prev) p1) = S9 prev (Rule21 p1)

instance Reduce (S9 (S10 (S11 prev))) (S24 (S11 prev)) where
  reduce (S9 (S10 prev) p1) = S24 prev (Rule10 p1)

instance Reduce (S13 (S14 (S11 prev))) (S24 (S11 prev)) where
  reduce (S13 (S14 prev) p1) = S24 prev (Rule11 p1)

instance Reduce (S9 (S10 (S12 prev))) (S34 (S12 prev)) where
  reduce (S9 (S10 prev) p1) = S34 prev (Rule10 p1)

instance Reduce (S13 (S14 (S12 prev))) (S34 (S12 prev)) where
  reduce (S13 (S14 prev) p1) = S34 prev (Rule11 p1)

instance Reduce (S20 (S21 (S14 prev))) (S13 (S14 prev)) where
  reduce (S20 (S21 prev) p1) = S13 prev (Rule30 p1)

instance Reduce (S23 (S4 (S14 prev))) (S13 (S14 prev)) where
  reduce (S23 (S4 prev) p1) = S13 prev (Rule31 p1)

instance Reduce (S24 (S11 (S16 prev))) (S15 (S16 prev)) where
  reduce (S24 (S11 prev) p1) = S15 prev (Rule40 p1)

instance Reduce (S26 (S17 (S16 prev))) (S15 (S16 prev)) where
  reduce (S26 (S17 prev) p1) = S15 prev (Rule41 p1)

instance Reduce (S15 (S16 (S17 prev))) (S26 (S17 prev)) where
  reduce (S15 (S16 prev) p1) = S26 prev (Rule20 p1)

instance Reduce (S18 (S19 (S17 prev))) (S26 (S17 prev)) where
  reduce (S18 (S19 prev) p1) = S26 prev (Rule21 p1)

instance Reduce (S27 (S22 (S19 prev))) (S18 (S19 prev)) where
  reduce (S27 (S22 prev) p1) = S18 prev (Rule50 p1)

instance Reduce (S29 (S25 (S19 prev))) (S18 (S19 prev)) where
  reduce (S29 (S25 prev) p1) = S18 prev (Rule51 p1)

instance Reduce (S30 (S28 (S21 prev))) (S20 (S21 prev)) where
  reduce (S30 (S28 prev) p1) = S20 prev (Rule60 p1)

instance Reduce (S32 (S31 (S21 prev))) (S20 (S21 prev)) where
  reduce (S32 (S31 prev) p1) = S20 prev (Rule61 p1)

instance Reduce (S20 (S21 (S22 prev))) (S27 (S22 prev)) where
  reduce (S20 (S21 prev) p1) = S27 prev (Rule30 p1)

instance Reduce (S23 (S4 (S22 prev))) (S27 (S22 prev)) where
  reduce (S23 (S4 prev) p1) = S27 prev (Rule31 p1)

instance Reduce (S24 (S11 (S25 prev))) (S29 (S25 prev)) where
  reduce (S24 (S11 prev) p1) = S29 prev (Rule40 p1)

instance Reduce (S26 (S17 (S25 prev))) (S29 (S25 prev)) where
  reduce (S26 (S17 prev) p1) = S29 prev (Rule41 p1)

instance Reduce (S27 (S22 (S28 prev))) (S30 (S28 prev)) where
  reduce (S27 (S22 prev) p1) = S30 prev (Rule50 p1)

instance Reduce (S29 (S25 (S28 prev))) (S30 (S28 prev)) where
  reduce (S29 (S25 prev) p1) = S30 prev (Rule51 p1)

instance Reduce (S30 (S28 (S31 prev))) (S32 (S31 prev)) where
  reduce (S30 (S28 prev) p1) = S32 prev (Rule60 p1)

instance Reduce (S32 (S31 (S31 prev))) (S32 (S31 prev)) where
  reduce (S32 (S31 prev) p1) = S32 prev (Rule61 p1)


-- Reduce -> Transition

instance {-# OVERLAPS #-} (Reduce r s, ITransition s t) =>
    ITransition r t where
  i r = i (reduce r)

instance {-# OVERLAPS #-} (Reduce r s, OTransition s t) =>
    OTransition r t where
  o r = o (reduce r)

-------------------------------------------------------------------------------

-- initial state

begin :: Program r (S2 ())
begin = program (S2 ())

-------------------------------------------------------------------------------

class End s r | s -> r where
  end :: s -> r

instance {-# OVERLAPS #-} End (S1 prev) S where
  end (S1 _ r) = r

instance {-# OVERLAPS #-} (Reduce r s, End s t) => End r t where
  end r = end (reduce r)

-------------------------------------------------------------------------------

