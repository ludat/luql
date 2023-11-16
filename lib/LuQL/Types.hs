{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module LuQL.Types where

import Data.Aeson
import Data.Kind (Constraint, Type)
import Data.Text (Text)

import GHC.Generics (Generic)
import GHC.TypeLits

type family StmtE (field :: Symbol) (purpose :: Symbol) type_

data QueryStatement stype
  = From (StmtE "from" "ctx" stype) (StmtE "from" "model" stype)
  | Where (StmtE "where" "ctx" stype) (QueryExpression stype)
  | Join (StmtE "join" "ctx" stype) (StmtE "join" "model" stype) (StmtE "join" "on" stype)
  | Let (StmtE "let" "ctx" stype) Text (QueryExpression stype)
  | GroupBy (StmtE "groupby" "ctx" stype) [(QueryExpression stype, Maybe Text)] [QueryStatement stype]
  | OrderBy (StmtE "orderby" "ctx" stype) [(QueryExpression stype, Maybe OrderDirection)]
  | Return (StmtE "return" "ctx" stype) [QueryExpression stype]
  | StmtExt (StmtE "ext" "ext" stype)
  deriving (Generic)

data OrderDirection
  = Asc
  | Desc
  deriving (Show, Eq, Generic)

instance ToJSON OrderDirection

type ForAllStmt (c :: Type -> Constraint) stype =
  ( c (StmtE "from" "ctx" stype),
    c (StmtE "where" "ctx" stype),
    c (StmtE "join" "ctx" stype),
    c (StmtE "let" "ctx" stype),
    c (StmtE "groupby" "ctx" stype),
    c (StmtE "orderby" "ctx" stype),
    c (StmtE "return" "ctx" stype),
    c (StmtE "ext" "ext" stype),
    c (StmtE "from" "model" stype),
    c (StmtE "join" "model" stype),
    c (StmtE "join" "on" stype),
    c (QueryExpression stype)
  )

deriving instance (ForAllStmt Show qtype) => Show (QueryStatement qtype)

deriving instance (ForAllStmt Eq qtype) => Eq (QueryStatement qtype)

data QueryExpression etype
  = Lit (ExprE "lit" "ctx" etype) LiteralValue
  | Ref (ExprE "ref" "ctx" etype) Text
  | Apply (ExprE "apply" "ctx" etype) (ExprE "apply" "function" etype) [QueryExpression etype]
  | Prop (ExprE "prop" "ctx" etype) (QueryExpression etype) Text
  | If (ExprE "if" "ctx" etype) (QueryExpression etype) (QueryExpression etype) (QueryExpression etype)
  | RawSql (ExprE "raw" "ctx" etype) [Either Text (QueryExpression etype)]
  | ExprExt (ExprE "ext" "ext" etype)
  deriving (Generic)

type family ExprE (field :: Symbol) (purpose :: Symbol) etype

data LiteralValue
  = LiteralString Text
  | LiteralInt Int
  | LiteralFloat Float
  | LiteralBoolean Bool
  | LiteralNull
  deriving (Show, Eq, Generic)

instance ToJSON LiteralValue

type ForAllExpr (c :: Type -> Constraint) etype =
  ( c (ExprE "lit" "ctx" etype),
    c (ExprE "ref" "ctx" etype),
    c (ExprE "apply" "ctx" etype),
    c (ExprE "apply" "function" etype),
    c (ExprE "prop" "ctx" etype),
    c (ExprE "if" "ctx" etype),
    c (ExprE "raw" "ctx" etype),
    c (ExprE "ext" "ext" etype)
  )

deriving instance (ForAllExpr Show etype) => Show (QueryExpression etype)

deriving instance (ForAllExpr Eq etype) => Eq (QueryExpression etype)
