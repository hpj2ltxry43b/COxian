{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR
    ( IRCtx

    , ds_interner
    , v_interner
    , function_interner

    , DSIdx
    , VIdx
    , upcast_dsidx
    , upcast_vidx
    , resolve_dsidx
    , resolve_vidx
    , apply_to_ds
    , apply_to_v

    , DeclSymbol'
    , Module
    , Type(..)

    , Signedness(..)
    , FloatSize(..)
    , IntSize(..)

    , Value'
    , ConstFunctionPointer
    , get_function_idx

    , type_of

    , Function
    , get_ret_type
    , get_param_types
    ) where

import IR.ChildList

import IR.DeclSpan
import IR.Typed

import IR.DeclSymbol
import IR.Module
import IR.Type

import IR.Value
import IR.ConstFunctionPointer

import IR.Function

import IR.ApplyTo

import IR.IRCtx

import qualified AST

import qualified Message
import qualified Message.Underlines as MsgUnds

import Location

import Interner
import SimpleLens

import qualified Control.Monad.State as State (State, state, get, runState)
import qualified Control.Monad.Reader as Reader (Reader, reader)

-- IRBuilder {{{1
data IRBuilder = IRBuilder IRCtx [IRBuildError]

irb_irctx :: Lens IRBuilder IRCtx
irb_irctx = Lens _ _
irb_errors :: Lens IRBuilder [IRBuildError]
irb_errors = Lens _ _
-- IRBuildError {{{1
data IRBuildError
    = DuplicateValue String Value' Value'
    | DuplicateLocal Function Local LValue
    | Unimplemented String Span
    | NotAType Span DeclSymbol'
    | PathDoesntExist Span -- TODO: change to 'no entity called x in y'
    | InvalidAssign Span Span
    | TypeError TypeError
    | AddrofNotLValue Span

duplicate_msg :: String -> String -> String -> Maybe Span -> Maybe Span -> Message.SimpleDiag
duplicate_msg entity_kind diag_name name old_sp new_sp =
    let if_span m_sp ty imp msg =
            case m_sp of
                Just sp -> Right $ MsgUnds.Underline sp imp [MsgUnds.Message ty msg]
                Nothing -> Left $ Message.Note msg

        oldmsg = if_span old_sp MsgUnds.Note MsgUnds.Secondary $ entity_kind ++ " '" ++ name ++ "' already declared"
        newmsg = if_span new_sp MsgUnds.Error MsgUnds.Primary $ entity_kind ++ " '" ++ name ++ "' redeclared"
        totalmsgs = [oldmsg, newmsg]

        underlines_section =
            case [x | Right x <- totalmsgs] of
                [] -> Nothing
                msgs -> Just $ Message.Underlines msgs
        notes = [Just x | Left x <- totalmsgs]
        sections = catMaybes $ underlines_section : notes
    in Message.SimpleDiag Message.Error new_sp Nothing (Just diag_name) sections

instance Message.ToDiagnostic (IRBuildError, IRCtx) where
    to_diagnostic (DuplicateValue name old new, irctx) =
        duplicate_msg "value" "redecl-val" name (decl_span irctx old) (decl_span irctx new)

    to_diagnostic (DuplicateLocal fun (Local name old_lvalue _) new_lvalue, irctx) =
        duplicate_msg "local" "redecl-local" name (decl_span irctx (fun, old_lvalue)) (decl_span irctx (fun, new_lvalue))

    to_diagnostic (Unimplemented name sp, _) =
        Message.SimpleDiag Message.Error (Just sp) Nothing Nothing
            [ Message.Underlines
                [ MsgUnds.Underline sp MsgUnds.Primary [MsgUnds.Message MsgUnds.Error $ "use of unimplemented feature: " ++ name]
                ]
            ]

    to_diagnostic (NotAType path_sp _, _) =
        Message.SimpleDiag Message.Error (Just path_sp) Nothing (Just "not-type")
            [ Message.Underlines
                [ MsgUnds.Underline path_sp MsgUnds.Primary [MsgUnds.Message MsgUnds.Error "not a type"]
                -- , MsgUnds.Underline path_sp MsgUnds.Secondary [MsgUnds.Message MsgUnds.Note $ "this path resolved to " ++ describe irctx ds] -- TODO: get entity type name
                ]
            ]

    to_diagnostic (PathDoesntExist path_sp, _) =
        Message.SimpleDiag Message.Error (Just path_sp) Nothing (Just "path-doesnt-exist")
            [ Message.Underlines
                [ MsgUnds.Underline path_sp MsgUnds.Primary [MsgUnds.Message MsgUnds.Error "entity referred to by path doesn't exist"]
                ]
            ]

    to_diagnostic (InvalidAssign target_sp op_sp, _) =
        Message.SimpleDiag Message.Error (Just op_sp) Nothing (Just "invalid-assign")
            [ Message.Underlines
                [ MsgUnds.Underline target_sp MsgUnds.Primary [MsgUnds.Message MsgUnds.Error "cannot assign to non-lvalue"]
                ]
            ]

    to_diagnostic (TypeError te, irctx) = Message.to_diagnostic (te, irctx)

    to_diagnostic (AddrofNotLValue sp, _) =
        Message.SimpleDiag Message.Error (Just sp) Nothing (Just "bad-ref")
            [ Message.Underlines
                [ MsgUnds.Underline sp MsgUnds.Primary [MsgUnds.Message MsgUnds.Error "cannot take pointer to non-lvalue"]
                ]
            ]

