#!/usr/bin/env runhaskell
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List

import qualified Text.GraphQL.Schema as GQ
import qualified Text.GraphQL.Schema.Graphene as Graphene

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
      GQ.IntType @?= GQ.IntType

   , testCase "FloatType equality" $
      GQ.FloatType @?= GQ.FloatType

   , testCase "BooleanType equality" $
      GQ.BooleanType @?= GQ.BooleanType

   , testCase "IDType equality" $
      GQ.IDType @?= GQ.IDType

   , testCase "StringType equality" $
      GQ.StringType @?= GQ.StringType

   , testCase "NonNull equality" $
      GQ.NonNull GQ.StringType @?= GQ.NonNull GQ.StringType

   , testCase "List equality" $
      GQ.ListType (GQ.NonNull GQ.FloatType) @?=
      GQ.ListType (GQ.NonNull GQ.FloatType)

   , testCase "EnumValue equality" $
      GQ.EnumValue { GQ.evName="", GQ.evValue="1" } @?=
      GQ.EnumValue { GQ.evName="", GQ.evValue="1" }

   , testCase "Enum equality" $
      GQ.EnumType GQ.EnumDef { GQ.enumName="", GQ.enumValues=[] } @?=
      GQ.EnumType GQ.EnumDef { GQ.enumName="", GQ.enumValues=[] }

   , testCase "ObjectField equality" $
      GQ.ObjectField { GQ.fieldName="", GQ.fieldType=GQ.StringType } @?=
      GQ.ObjectField { GQ.fieldName="", GQ.fieldType=GQ.StringType }

   , testCase "Graphene Render Smoke" $
      Graphene.render GQ.ObjectType {
         GQ.objName = "MyObj",
         GQ.objFields = [GQ.ObjectField{
               GQ.fieldName = "first_name",
               GQ.fieldType = GQ.StringType
            }]
      }
      @?=
      ("class MyObj(graphene.ObjectType):\n" ++
      "    first_name = graphene.String()")

   , testCase "Graphene Render Full" $
      Graphene.render GQ.ObjectType {
         GQ.objName = "MyObj",
         GQ.objFields = [
            GQ.ObjectField{
               GQ.fieldName = "a_float",
               GQ.fieldType = GQ.FloatType
            },
            GQ.ObjectField{
               GQ.fieldName = "a_int",
               GQ.fieldType = GQ.IntType
            },
            GQ.ObjectField{
               GQ.fieldName = "a_bool",
               GQ.fieldType = GQ.BooleanType
            }
         ]
      }
      @?=
      ("class MyObj(graphene.ObjectType):\n" ++
      "    a_float = graphene.Float()\n" ++
      "    a_int = graphene.Int()\n" ++
      "    a_bool = graphene.Boolean()")

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
