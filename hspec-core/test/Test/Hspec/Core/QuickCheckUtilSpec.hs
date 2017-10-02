{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hspec.Core.QuickCheckUtilSpec (spec) where

import           Helper

import qualified Test.QuickCheck.Property as QCP

import           Test.Hspec.Core.QuickCheckUtil

deriving instance Eq QuickCheckResult
deriving instance Eq Status
deriving instance Eq QuickCheckFailure

spec :: Spec
spec = do
  describe "formatNumbers" $ do
    it "includes number of tests" $ do
      formatNumbers 1 0 `shouldBe` "(after 1 test)"

    it "pluralizes number of tests" $ do
      formatNumbers 3 0 `shouldBe` "(after 3 tests)"

    it "includes number of shrinks" $ do
      formatNumbers 3 1 `shouldBe` "(after 3 tests and 1 shrink)"

    it "pluralizes number of shrinks" $ do
      formatNumbers 3 3 `shouldBe` "(after 3 tests and 3 shrinks)"

  describe "stripSuffix" $ do
    it "drops the given suffix from a list" $ do
      stripSuffix "bar" "foobar" `shouldBe` Just "foo"

  describe "parseQuickCheckResult" $ do
    let qc = quickCheckWithResult stdArgs {chatty = False, replay = Just (mkGen 0, 0)}

    context "with Success" $ do
      let p :: Int -> Bool
          p n = n == n

      it "parses result" $ do
        parseQuickCheckResult <$> qc p `shouldReturn`
          QuickCheckResult 100 "+++ OK, passed 100 tests.\n" QuickCheckSuccess

      it "includes labels" $ do
        parseQuickCheckResult <$> qc (label "unit" p) `shouldReturn`
          QuickCheckResult 100 "+++ OK, passed 100 tests (100% unit).\n" QuickCheckSuccess

    context "with GaveUp" $ do
      let p :: Int -> Property
          p n = (n == 1234) ==> True

      it "parses result" $ do
        parseQuickCheckResult <$> qc p `shouldReturn`
          QuickCheckResult 0 "" (QuickCheckOtherFailure "Gave up after 0 tests")

    context "with NoExpectedFailure" $ do
      let p :: Int -> Property
          p _ = expectFailure True

      it "parses result" $ do
        parseQuickCheckResult <$> qc p `shouldReturn`
          QuickCheckResult 100 "" (QuickCheckOtherFailure "Passed 100 tests (expected failure)")

    context "with InsufficientCoverage" $ do
      let p :: Int -> Property
          p n = cover (n == 23) 10 "is 23" True

      it "parses result" $ do
        parseQuickCheckResult <$> qc p `shouldReturn`
          QuickCheckResult 100 "" (QuickCheckOtherFailure "Insufficient coverage after 100 tests (only 0% is 23, not 10%).")

    context "with Failure" $ do
      context "with single-line failure reason" $ do
        let
          p :: Int -> Bool
          p = (/= 1)

          err = "Falsifiable"
          result = QuickCheckResult 2 "" (QuickCheckFailure $ QCFailure 0 Nothing err ["1"])

        it "parses result" $ do
          parseQuickCheckResult <$> qc p `shouldReturn` result

        it "includes verbose output" $ do
          let info = unlines [
                  "Passed:"
                , "0"
                , ""
                , "Failed:"
                , "1"
                , ""
                , "*** Failed! Passed:"
                , "0"
                , ""
                ]

          parseQuickCheckResult <$> qc (verbose p) `shouldReturn` result {quickCheckResultInformal = info}

      context "with multi-line failure reason" $ do
        let
          p :: Int -> QCP.Result
          p n = if n /= 1 then QCP.succeeded else QCP.failed {QCP.reason = err}

          err = "foo\nbar"
          result = QuickCheckResult 2 "" (QuickCheckFailure $ QCFailure 0 Nothing err ["1"])

        it "parses result" $ do
          parseQuickCheckResult <$> qc p `shouldReturn` result

        it "includes verbose output" $ do
          let info = unlines [
                  "Passed:"
                , "0"
                , ""
                , "Failed:"
                , "1"
                , ""
                , "*** Failed! Passed:"
                , "0"
                , ""
                ]
          parseQuickCheckResult <$> qc (verbose p) `shouldReturn` result {quickCheckResultInformal = info}

      context "with HUnit assertion" $ do
        let p :: Int -> Int -> Expectation
            p m n = do
              m `shouldBe` n

        it "includes counterexample" $ do
          result <- qc p
          let QuickCheckResult _ _ (QuickCheckFailure r) = parseQuickCheckResult result
          quickCheckFailureCounterexample r `shouldBe` ["0", "1"]
