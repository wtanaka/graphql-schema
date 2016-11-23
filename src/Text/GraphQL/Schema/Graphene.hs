#!/usr/bin/env runhaskell
--
-- Copyright 2016 Wesley Tanaka <http://wtanaka.com/>
--
-- This file is part of graphql-schema
--
-- graphql-schema is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- graphql-schema is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with graphql-schema.  If not, see <http://www.gnu.org/licenses/>.

{-|
Module: Text.GraphQL.Graphene
Description: Generate graphene-python schema code
Copyright: (C) Wesley Tanaka <http://wtanaka.com/>, 2016
License: GPL-2
Stability: experimental
Portability: POSIX

Toy support for generating graphene-python code
-}
module Text.GraphQL.Schema.Graphene
   (
      -- |Render a GraphQL type as graphene code
      render
   ) where

import qualified Language.Python.Common as PY
import qualified Text.GraphQL.Schema as GQ

graphene :: PY.Expr PY.SrcSpan
graphene = var "graphene"

grapheneDot :: PY.Ident PY.SrcSpan -> PY.Expr PY.SrcSpan
grapheneDot x = PY.Dot {
   PY.dot_expr = graphene,
   PY.dot_attribute = x,
   PY.expr_annot = PY.SpanEmpty
}

call :: PY.Expr PY.SrcSpan -> PY.Expr PY.SrcSpan
call x = PY.Call {
   PY.call_fun = x,
   PY.call_args = [],
   PY.expr_annot = PY.SpanEmpty
}

ident :: String -> PY.Ident PY.SrcSpan
ident x = PY.Ident { PY.ident_string = x, PY.ident_annot = PY.SpanEmpty }

var :: String -> PY.Expr PY.SrcSpan
var x = PY.Var { PY.var_ident = ident x, PY.expr_annot = PY.SpanEmpty }

wrap :: PY.Expr PY.SrcSpan -> PY.Expr PY.SrcSpan -> PY.Expr PY.SrcSpan
wrap outer inner = PY.Call {
   PY.call_fun = outer,
   PY.call_args = [PY.ArgExpr {
      PY.arg_expr = inner,
      PY.arg_annot = PY.SpanEmpty
   }],
   PY.expr_annot = PY.SpanEmpty
}

grapheneType :: GQ.Type -> PY.Expr PY.SrcSpan
grapheneType GQ.BooleanType = call $ grapheneDot $ ident "Boolean"
grapheneType GQ.FloatType = call $ grapheneDot $ ident "Float"
grapheneType GQ.IDType = call $ grapheneDot $ ident "ID"
grapheneType GQ.IntType = call $ grapheneDot $ ident "Int"
grapheneType (GQ.ListType x) = wrap (call $ grapheneDot $ ident "List") (grapheneType x)
grapheneType (GQ.NonNull x) = wrap (call $ grapheneDot $ ident "NonNull") (grapheneType x)
grapheneType GQ.StringType = call $ grapheneDot $ ident "String"

grapheneTypeDef :: GQ.TypeDef -> PY.Statement PY.SrcSpan
grapheneTypeDef GQ.ObjectType {
   GQ.objName = on,
   GQ.objFields = ofs
   } = PY.Class {
      PY.class_name = ident on,
      PY.class_args = argList [grapheneDot $ ident "ObjectType"],
      PY.class_body = classBody ofs,
      PY.stmt_annot = PY.SpanEmpty
      }
grapheneTypeDef (GQ.EnumType GQ.EnumDef {
      GQ.enumName = enumName,
      GQ.enumValues = enumValues
      }) =
      PY.Class {
         PY.class_name = ident enumName,
         PY.class_args = argList [grapheneDot $ ident "Enum"],
         PY.class_body = enumClassBody enumValues,
         PY.stmt_annot = PY.SpanEmpty
      }

pyField :: GQ.ObjectField -> PY.Statement PY.SrcSpan
pyField objF = PY.Assign {
   PY.assign_to = [var $ GQ.fieldName objF],
   PY.assign_expr = grapheneType $ GQ.fieldType objF,
   PY.stmt_annot = PY.SpanEmpty
}

classBody :: [GQ.ObjectField] -> [PY.Statement PY.SrcSpan]
classBody = map pyField

grapheneVal :: GQ.EvVal -> PY.Expr PY.SrcSpan
grapheneVal (GQ.EvStr s) = PY.Strings {
   PY.strings_strings = [s],
   PY.expr_annot = PY.SpanEmpty
}
grapheneVal (GQ.EvInt i) = PY.Int { PY.int_value = i,
   -- TODO: Questionable
   PY.expr_literal = show i,
   PY.expr_annot = PY.SpanEmpty
}

pyEnumField :: GQ.EnumValue -> PY.Statement PY.SrcSpan
pyEnumField eV = PY.Assign {
   PY.assign_to = [var $ GQ.evName eV],
   PY.assign_expr = grapheneVal $ GQ.evValue eV,
   PY.stmt_annot = PY.SpanEmpty
}

enumClassBody :: [GQ.EnumValue] -> [PY.Statement PY.SrcSpan]
enumClassBody = map pyEnumField

argList :: [PY.Expr PY.SrcSpan] -> [PY.Argument PY.SrcSpan]
argList = foldr
   (\ x -> (:) PY.ArgExpr{PY.arg_expr = x, PY.arg_annot = PY.SpanEmpty})
   ([] :: [PY.Argument PY.SrcSpan])

listify :: a -> [a]
listify a = [a]

render :: GQ.TypeDef -> String
render = PY.prettyText . PY.Module . listify . grapheneTypeDef
-- render _ = "import graphene"
