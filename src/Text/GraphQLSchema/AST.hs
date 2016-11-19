#!/usr/bin/env runhaskell
--
-- Copyright 2016 Wesley Tanaka <http://wtanaka.com/>
--
-- This file is part of graphqlschema
--
-- graphqlschema is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- graphqlschema is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with graphqlschema.  If not, see <http://www.gnu.org/licenses/>.

module Text.GraphQLSchema.AST
   (
      Type(..),
      EnumValue(..),
      ObjectField(..),
   ) where

data Type = IntType
   | FloatType
   | StringType
   | BooleanType
   | IDType
   | NonNull Type
   | ListType Type
   | EnumType {
      enumName :: String,
      enumValues :: [EnumValue]
   }
   | ObjectType {
      objName :: String,
      objFields :: [ObjectField]
   }
   deriving (Show,Read,Eq)

data EnumValue = EnumValue {
   evName :: String,
   evValue :: String
   } deriving (Show,Read,Eq)

data ObjectField = ObjectField {
   fieldName :: String,
   fieldType :: Type
   } deriving (Show,Read,Eq)
