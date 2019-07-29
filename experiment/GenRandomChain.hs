
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module GenRandomChain where

import qualified Data.Map as Map
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import Data.Either
import Text.Printf
import qualified System.Random.Mersenne.Pure64 as R
import qualified Syntax as S
import Utility

-------------------------------------------------------------------------------

-- count helper data structure

newtype Count = Count { getCount :: Map.Map Int Integer }
  deriving (Show, Eq, Ord)

getCountOf :: Int -> Count -> Integer
getCountOf k (Count c) = Map.findWithDefault 0 k c

emptyCount :: Count
emptyCount = Count Map.empty

makeCount :: Map.Map Int Integer -> Count
makeCount = Count . Map.filter (/= 0)

makeCountFromListWith :: (Integer -> Integer -> Integer) -> [(Int, Integer)] -> Count
makeCountFromListWith f = makeCount . Map.fromListWith f

makeCountFromList :: [(Int, Integer)] -> Count
makeCountFromList = makeCountFromListWith const

singleCount :: Int -> Count
singleCount x = Count (Map.singleton x 1)

count :: (Foldable t) => (a -> Int) -> t a -> Count
count f xs = foldr (\a c -> singleCount (f a) + c) emptyCount xs

instance Num Count where
  Count c1 + Count c2 = makeCount (Map.unionWith (+) c1 c2)
  Count c1 * Count c2 = Count . Map.fromListWith (+) $ do
    ((k1, v1), (k2, v2)) <- (,) <$> Map.toList c1 <*> Map.toList c2
    return (k1 + k2, v1 * v2)
  negate (Count c) = Count (Map.map negate c)
  abs    (Count c) = Count (Map.map abs    c)
  signum (Count c) = Count (Map.map signum c)
  fromInteger i = if i == 0 then emptyCount
                  else Count (Map.singleton 0 (fromInteger i))

-------------------------------------------------------------------------------

-- grammar definition

type Grammar nt t = Map.Map nt [[Either nt t]]

-------------------------------------------------------------------------------

-- preprocessing

data GenEnv nt t = GenEnv { genEnvGrammar  :: Grammar nt t,
                            genEnvCounts   :: Map.Map nt Count,
                            genEnvMaxCount :: Int }

initialGenEnv :: (Ord nt) => Grammar nt t -> GenEnv nt t
initialGenEnv grammar = GenEnv grammar (initialCounts grammar) (-1)

initialCounts :: (Ord nt) => Grammar nt t -> Map.Map nt Count
initialCounts grammar = Map.fromList $ do
  (nt, rules) <- Map.toList grammar
  return (nt, rules |> filter (all isRight) |> count length)

limitCounts :: Int -> Map.Map nt Count -> Map.Map nt Count
limitCounts n = Map.map $ \count -> case count of
  Count c -> Count (Map.filterWithKey (\k _ -> k <= n) c)

countNonTerminal :: (Ord nt) => Grammar nt t -> Map.Map nt Count -> nt -> Count
countNonTerminal grammar counts nt = sum $
  [countSymbols grammar counts symbols | symbols <- grammar Map.! nt]

countSymbols :: (Ord nt) => Grammar nt t -> Map.Map nt Count -> [Either nt t] -> Count
countSymbols grammar counts symbols = product $
  [countSymbol grammar counts symbol | symbol <- symbols]

countSymbol :: (Ord nt) => Grammar nt t -> Map.Map nt Count -> Either nt t -> Count
countSymbol _ counts symbol = case symbol of
  Left  nt -> counts Map.! nt
  Right _  -> singleCount 1

growCounts :: (Ord nt) => Grammar nt t -> Map.Map nt Count -> Map.Map nt Count
growCounts grammar counts = Map.fromList $
  [(nt, countNonTerminal grammar counts nt) | nt <- Map.keys grammar]

growCountsTo :: (Ord nt) => Int -> GenEnv nt t -> GenEnv nt t
growCountsTo n env = if n <= maxCount then env
                     else env { genEnvCounts   = grow counts,
                                genEnvMaxCount = n }
  where grammar  = genEnvGrammar  env
        counts   = genEnvCounts   env
        maxCount = genEnvMaxCount env
        grow     = fixPoint (limitCounts n . growCounts grammar)

-------------------------------------------------------------------------------

-- preprocessed data (GenEnv) interface

withdraw :: (Ord nt, MonadState (GenEnv nt t) m) =>
            (Grammar nt t -> Map.Map nt Count -> k -> Count) ->
            k -> Int -> m Integer
withdraw getter k n = do
  modify (growCountsTo n)
  grammar <- gets genEnvGrammar
  counts  <- gets genEnvCounts
  return (getCountOf n (getter grammar counts k))

withdrawCountNonTerminal :: (Ord nt, MonadState (GenEnv nt t) m) => nt -> Int -> m Integer
withdrawCountNonTerminal = withdraw countNonTerminal

withdrawCountSymbols :: (Ord nt, MonadState (GenEnv nt t) m) => [Either nt t] -> Int -> m Integer
withdrawCountSymbols = withdraw countSymbols

-------------------------------------------------------------------------------

-- random monad

popAt :: Int -> [a] -> (a, [a])
popAt _ [] = error "popAt: index out of range"
popAt i (x : xs) = if i == 0 then (x, xs)
                   else let (y, ys) = popAt (i - 1) xs in (y, x : ys)

class (Monad m) => MonadRandom m where
  randomDouble :: m Double
  randomRange  :: (Integral a) => a -> a -> m a
  randomPop    :: [a] -> m (a, [a])
  choice       :: [a] -> m a
  shuffle      :: [a] -> m [a]

  randomRange l h
    | l >= h    = error "MonadRandom.randomRange: empty range"
    | otherwise = do r <- randomDouble
                     return (l + floor (r * fromIntegral (h - l)))
  randomPop xs = do i <- randomRange 0 (length xs)
                    return (popAt i xs)
  choice = fmap fst . randomPop
  shuffle xs = do (y, ys) <- randomPop xs
                  zs <- shuffle ys
                  return (y : zs)

  {-# MINIMAL randomDouble #-}

newtype RandomT m a = RandomT { getRandomT :: StateT R.PureMT m a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadTrans,
            MonadIO)

instance (Monad m) => MonadRandom (RandomT m) where
  randomDouble = RandomT (state R.randomDouble)

instance (MonadWriter w m) => MonadWriter w (RandomT m) where
  writer aw = lift (writer aw)
  tell   w  = lift (tell w)
  listen m  = RandomT (listen (getRandomT m))
  pass   m  = RandomT (pass   (getRandomT m))

instance (MonadState s m) => MonadState s (RandomT m) where
  state = lift . state

instance (MonadRandom m, Monoid w) => MonadRandom (WriterT w m) where
  randomDouble = lift randomDouble

instance (MonadRandom m) => MonadRandom (StateT s m) where
  randomDouble = lift randomDouble

runRandomT :: (MonadIO m) => RandomT m a -> m a
runRandomT m = liftIO R.newPureMT >>= evalStateT (getRandomT m)

type Random = RandomT Identity

runRandom :: Random a -> IO a
runRandom m = evalState (getRandomT m) <$> R.newPureMT

wchoice :: (Integral a, MonadRandom m) => [(a, b)] -> m b
wchoice cands = fmap (\r -> go r cands) (randomRange 0 wsum)
  where wsum = sum (map fst cands)
        go _ [(_, b)]        = b
        go r ((w, b) : rest) = if r < w then b else go (r - w) rest

-------------------------------------------------------------------------------

-- random sequence generation

genSymbol :: (Ord nt, MonadState (GenEnv nt t) m, MonadRandom m) =>
             Either nt t -> Int -> m [t]
genSymbol symbol n = case symbol of
  Left  nt -> genNonTerminal nt n
  Right t  -> return [t]  -- assert n == 1

genSymbols :: (Ord nt, MonadState (GenEnv nt t) m, MonadRandom m) =>
              [Either nt t] -> Int -> m [t]
genSymbols []             n = return []  -- assert n == 0
genSymbols (first : rest) n = do
  candidates <- forM [0 .. n] $ \i -> do
    wFirst <- withdraw countSymbol  first i
    wRest  <- withdraw countSymbols rest  (n - i)
    return (wFirst * wRest, i)
  i <- wchoice candidates
  former <- genSymbol  first i
  latter <- genSymbols rest (n - i)
  return (former ++ latter)

genNonTerminal :: (Ord nt, MonadState (GenEnv nt t) m, MonadRandom m) =>
                  nt -> Int -> m [t]
genNonTerminal nt n = do
  grammar <- gets genEnvGrammar
  candidates <- forM (grammar Map.! nt) $ \symbols -> do
    w <- withdraw countSymbols symbols n
    return (w, symbols)
  symbols <- wchoice candidates
  genSymbols symbols n

gen :: (Ord nt, MonadState (GenEnv nt t) m, MonadRandom m) => nt -> Int -> m [t]
gen = genNonTerminal

-------------------------------------------------------------------------------

-- sample grammars

g1 :: Grammar String Char
g1 = Map.fromList [("S", [[Right 'a'],
                          [Left "S", Left "S"]])]

g2 :: Grammar String Char
g2 = Map.fromList [("E", [[Left "E", Right '+', Left "T"],
                          [Left "E", Right '-', Left "T"],
                          [Left "T"]]),
                   ("T", [[Left "T", Right '*', Left "F"],
                          [Left "T", Right '/', Left "F"],
                          [Left "F"]]),
                   ("F", [[Left "num"],
                          [Right '(', Left "E", Right ')']]),
                   ("num", [[Right '0'],
                            [Right '1', Left "num0"],
                            [Right '2', Left "num0"],
                            [Right '3', Left "num0"],
                            [Right '4', Left "num0"],
                            [Right '5', Left "num0"],
                            [Right '6', Left "num0"],
                            [Right '7', Left "num0"],
                            [Right '8', Left "num0"],
                            [Right '9', Left "num0"]]),
                   ("num0", [[],
                             [Right '0', Left "num0"],
                             [Right '1', Left "num0"],
                             [Right '2', Left "num0"],
                             [Right '3', Left "num0"],
                             [Right '4', Left "num0"],
                             [Right '5', Left "num0"],
                             [Right '6', Left "num0"],
                             [Right '7', Left "num0"],
                             [Right '8', Left "num0"],
                             [Right '9', Left "num0"]])]

-------------------------------------------------------------------------------

syntaxToGrammar :: S.Syntax -> (S.NonTerminal, Grammar S.NonTerminal S.Terminal)
syntaxToGrammar syntax = (S.syntaxStart syntax, go (S.syntaxRulesTable syntax))
  where go = fmap (map (map convertSymbol . S.ruleRhs))
        convertSymbol (S.NonTerminalSymbol nt) = Left nt
        convertSymbol (S.TerminalSymbol t) = Right t

genRandomChain :: (MonadIO m) => S.Syntax -> (S.NonTerminal -> StateT (GenEnv S.NonTerminal S.Terminal) (RandomT m) a) -> m a
genRandomChain syntax k = let (nt, grammar) = syntaxToGrammar syntax in
  runRandomT . (`evalStateT` initialGenEnv grammar) $ k nt

-------------------------------------------------------------------------------
