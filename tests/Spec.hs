#!/usr/bin/env runhaskell
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List

import qualified Text.GraphQLSchema.AST as AST

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , SC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^(7 :: Integer) - x) `mod` 7 == 0
  -- the following property does not hold
  --, SC.testProperty "Fermat's last theorem" $
  --    \x y z n ->
  --      (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
  ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^(7 :: Integer) - x) `mod` 7 == 0
  -- the following property does not hold
  -- , QC.testProperty "Fermat's last theorem" $
  --     \x y z n ->
  --       (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "IntType equality" $
      AST.IntType @?= AST.IntType

   , testCase "FloatType equality" $
      AST.FloatType @?= AST.FloatType

   , testCase "BooleanType equality" $
      AST.BooleanType @?= AST.BooleanType

   , testCase "IDType equality" $
      AST.IDType @?= AST.IDType

   , testCase "StringType equality" $
      AST.StringType @?= AST.StringType

   , testCase "NonNull equality" $
      AST.NonNull AST.StringType @?= AST.NonNull AST.StringType

   , testCase "List equality" $
      AST.ListType (AST.NonNull AST.FloatType) @?=
      AST.ListType (AST.NonNull AST.FloatType)

   , testCase "EnumValue equality" $
      AST.EnumValue { AST.evName="", AST.evValue="1" } @?=
      AST.EnumValue { AST.evName="", AST.evValue="1" }

   , testCase "Enum equality" $
      AST.EnumType { AST.enumName="", AST.enumValues=[] } @?=
      AST.EnumType { AST.enumName="", AST.enumValues=[] }

   , testCase "ObjectField equality" $
      AST.ObjectField { AST.fieldName="", AST.fieldType=AST.StringType } @?=
      AST.ObjectField { AST.fieldName="", AST.fieldType=AST.StringType }

--   , testCase "PersonType example" $
--       Object ObjectFields {
--          objName = "Person",
--          objFields = [
--             ObjectField {
--                fieldName = "name",
--                fieldType = String
--             },
--             ObjectField {
--                fieldName = "bestFriend",
--                fieldType = String
--             }
--          ]
--       }

  -- the following test does not hold
  --, testCase "List comparison (same length)" $
  --    [1, 2, 3] `compare` [1,2,2] @?= LT
  ]

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

main :: IO ()
main = defaultMain tests
