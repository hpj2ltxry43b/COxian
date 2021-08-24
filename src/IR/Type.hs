{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Type
    ( Signedness(..)
    , FloatSize(..)
    , IntSize(..)
    , Type(..)

    , stringify_ty
    , match_signedness

    , builtin_types
    ) where

import IR.DeclSpan
import IR.DeclSymbol

import IR.IRCtx

import Data.List (intercalate)

import qualified Control.Monad.Reader as Reader (Reader)

data Signedness = Signed | Unsigned deriving Eq
data FloatSize = F32 | F64 deriving Eq
data IntSize = I8 | I16 | I32 | I64 deriving Eq
data Type
    = FloatType FloatSize
    | IntType IntSize Signedness
    | CharType
    | BoolType
    | UnitType
    | FunctionPointerType (DSIdx Type) [DSIdx Type]
    deriving Eq

instance Show FloatSize where
    show F32 = "32"
    show F64 = "64"

instance Show IntSize where
    show I8 = "8"
    show I16 = "16"
    show I32 = "32"
    show I64 = "64"

instance DeclSpan IRCtx Type where
    decl_span (FloatType _) = return Nothing
    decl_span (IntType _ _) = return Nothing
    decl_span CharType = return Nothing
    decl_span BoolType = return Nothing
    decl_span UnitType = return Nothing
    decl_span (FunctionPointerType _ _) = return Nothing

instance IsDeclSymbol IRCtx Type

match_signedness :: a -> a -> Signedness -> a
match_signedness s u sgn =
    case sgn of
        Signed -> s
        Unsigned -> u

stringify_ty :: Type -> Reader.Reader IRCtx String

stringify_ty (FloatType F32) = return "float"
stringify_ty (FloatType F64) = return "double"

stringify_ty (IntType size signedness) = return $ match_signedness "s" "u" signedness ++ "int" ++ show size

stringify_ty CharType = return "char"
stringify_ty BoolType = return "bool"
stringify_ty UnitType = return "unit"

stringify_ty (FunctionPointerType retty params) =
    resolve_dsidx retty >>=
    stringify_ty >>= \ ret_str ->
    sequence (map (\ dsidx -> resolve_dsidx dsidx >>= stringify_ty) params) >>= \ param_strs ->
    return $ "fun(" ++ intercalate ", " param_strs ++ "): " ++ ret_str

builtin_types :: [(String, Type)]
builtin_types =
    [ ("unit", UnitType)
    , ("float", FloatType F32)
    , ("double", FloatType F64)
    , ("bool", BoolType)
    , ("char", CharType)
    , ("uint8", IntType I8 Unsigned)
    , ("uint16", IntType I16 Unsigned)
    , ("uint32", IntType I32 Unsigned)
    , ("uint64", IntType I64 Unsigned)
    , ("sint8", IntType I8 Signed)
    , ("sint16", IntType I16 Signed)
    , ("sint32", IntType I32 Signed)
    , ("sint64", IntType I64 Signed)
    ]
