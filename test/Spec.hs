
import Test.Hspec

import Syntax
import SyntaxParser

import qualified SampleSyntaxes
import qualified IntegratedTest

import Control.Monad  (forM_)
import Data.Either    (isRight)

-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "sample syntaxes" $ do
    it "should parse sample syntaxes" $ do
      forM_ SampleSyntaxes.sampleSyntaxSources $ \source -> do
        parse parseSyntax "" source `shouldSatisfy` isRight

    it "show and parseSyntax are isomorphic" $ do
      forM_ SampleSyntaxes.sampleSyntaxes $ \syntax -> do
        case parse parseSyntax "" (show syntax) of
          Left err -> expectationFailure (show err)
          Right syntax' -> show syntax' `shouldBe` show syntax

  describe "integrated test" $ do
    IntegratedTest.integratedSpec

-------------------------------------------------------------------------------
