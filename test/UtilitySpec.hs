
{-# LANGUAGE ScopedTypeVariables #-}

module UtilitySpec where

import Test.Hspec
import Test.QuickCheck
import Utility

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Cont
import Control.Arrow
import Data.List
import Control.Exception

-------------------------------------------------------------------------------

spec :: Spec
spec = describe "Utility" $ do
  fromLeftSpec
  fromRightSpec
  equalingSpec
  fixPointBySpec
  mapMWithSepSpec
  splitIdentifierSpec

-------------------------------------------------------------------------------

fromLeftSpec :: Spec
fromLeftSpec = describe "fromLeft" $ do
  it "fromLeft . Left == id" $ property $ \(x :: Int) -> do
    fromLeft (Left x :: Either Int Int) `shouldBe` x

  it "(fromLeft . Right) throws an error" $ property $ \(x :: Int) -> do
    fromLeft (Right x) `shouldThrow` (\(_ :: SomeException) -> True)

fromRightSpec :: Spec
fromRightSpec = describe "fromRight" $ do
  it "fromRight . Right == id" $ property $ \(x :: Int) -> do
    fromRight (Right x :: Either Int Int) `shouldBe` x

  it "(fromRight . Left) throws an error" $ property $ \(x :: Int) -> do
    fromRight (Left x) `shouldThrow` (\(_ :: SomeException) -> True)

-------------------------------------------------------------------------------

equalingSpec :: Spec
equalingSpec = describe "equaling" $ do
  it "" $ property $ \(x :: Int, y :: Int, z :: Int) -> do
    equaling fst (x, y) (x, z) `shouldBe` True
    equaling snd (x, z) (y, z) `shouldBe` True

  it "" $ property $ \(x :: Int, y :: Int) -> do
    equaling head [x] [y] `shouldBe` (x == y)
    equaling fst (x, ()) (y, ()) `shouldBe` (x == y)
    equaling snd ((), x) ((), y) `shouldBe` (x == y)

-------------------------------------------------------------------------------

fixPointBySpec :: Spec
fixPointBySpec = describe "fixPoint" $ do
  it "fibonacchi test" $ do
    let fibs n = fixPoint (\xs -> take n (0 : 1 : zipWith (+) xs (tail xs))) [0]
    fibs 20 `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181]
  it "pi-test" $ do
    let diff n a b = abs (a - b) < n
        calcPi = fixPointBy (equalingBy (diff 1e-5) snd) $ \(i, x) ->
          (i + 1, x + (-1) ^ i / fromIntegral (2 * i + 1))
        (_, pi_4) = calcPi (0, 0)
    4 * pi_4 `shouldSatisfy` diff (1e-4) pi

-------------------------------------------------------------------------------

mapMWithSepSpec :: Spec
mapMWithSepSpec = describe "mapMWithSep" $ do
  let told = second (`appEndo` "") . runWriter

  it "test on Writer #0" $ property $ \sep -> do
    told (mapMWithSep (tells sep) tells [])
      `shouldBe` ([], "")

  it "test on Writer #1" $ property $ \(sep, xs1) -> do
    told (mapMWithSep (tells sep) tells [xs1])
      `shouldBe` ([()], xs1)

  it "test on Writer #2" $ property $ \(sep, xs1, xs2) -> do
    told (mapMWithSep (tells sep) tells [xs1, xs2])
      `shouldBe` ([(), ()], xs1 ++ sep ++ xs2)

  it "test on Writer #3" $ property $ \(sep, xs1, xs2, xs3) -> do
    told (mapMWithSep (tells sep) tells [xs1, xs2, xs3])
      `shouldBe` ([(), (), ()], xs1 ++ sep ++ xs2 ++ sep ++ xs3)

  it "test on Writer #n" $ property $ \(sep, xss) -> do
    told (mapMWithSep (tells sep) tells xss)
      `shouldBe` (() <$ xss, intercalate sep xss)

-------------------------------------------------------------------------------

splitIdentifierSpec :: Spec
splitIdentifierSpec = describe "splitIdentifier" $ do
  it "works for camelCase" $ do
    splitIdentifier "camelCase" `shouldBe` ["", "camel", "Case", ""]
    splitIdentifier "splitIdentifierSpec" `shouldBe` ["", "split", "Identifier", "Spec", ""]

  it "works for PascalCase" $ do
    splitIdentifier "PascalCase" `shouldBe` ["",  "Pascal", "Case", ""]
    splitIdentifier "MonadTrans" `shouldBe` ["Monad", "Trans", ""]
    splitIdentifier "Int" `shouldBe` ["", "int", ""]

  it "works for snake_case" $ do
    splitIdentifier "snake_case" `shouldBe` ["", "snake", "case", ""]

  it "works for lisp-case" $ do
    splitIdentifier "lisp-case" `shouldBe` ["", "lisp", "case", ""]

  it "works for ALL_CAPS" $ do
    splitIdentifier "ALL_CAPS" `shouldBe` ["", "ALL", "CAPS", ""]

  it "works for edge case" $ do
    splitIdentifier "" `shouldBe` []
    splitIdentifier "ASTNode" `shouldBe` ["", "AST", "Node", ""]

-------------------------------------------------------------------------------
