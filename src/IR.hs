{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
import StateAndLens

import Data.Maybe (catMaybes)

import qualified Control.Monad.State as State (State, state, runState, get)
import qualified Control.Monad.Reader as Reader (Reader, reader, runReader)

-- IRBuilder {{{1
data IRBuilder = IRBuilder IRCtx [IRBuildError]

irb_irctx :: Lens IRBuilder IRCtx
irb_irctx = Lens (\ (IRBuilder i _) -> i)  (\ (IRBuilder _ e) i -> IRBuilder i e)
irb_errors :: Lens IRBuilder [IRBuildError]
irb_errors = Lens (\ (IRBuilder _ e) -> e) (\ (IRBuilder i _) e -> IRBuilder i e)
-- IRBuildError {{{1
data IRBuildError
    = DuplicateValue String Value' Value'
    -- | DuplicateLocal Function Local LValue TODO
    | Unimplemented String Span
    | NotAType Span DeclSymbol'
    | PathDoesntExist Span -- TODO: change to 'no entity called x in y'
    | InvalidAssign Span Span
    | TypeError TypeError
    | AddrofNotLValue Span

duplicate_msg :: (DeclSpan IRCtx a, DeclSpan IRCtx b) => String -> String -> String -> a -> b -> Reader.Reader IRCtx Message.SimpleDiag
duplicate_msg entity_kind diag_name name old new =
    decl_span old >>= \ old_sp ->
    decl_span new >>= \ new_sp ->
    let oldmsg = if_span old_sp MsgUnds.Note MsgUnds.Secondary $ entity_kind ++ " '" ++ name ++ "' already declared"
        newmsg = if_span new_sp MsgUnds.Error MsgUnds.Primary $ entity_kind ++ " '" ++ name ++ "' redeclared"
        totalmsgs = [oldmsg, newmsg]

        underlines_section =
            case [x | Right x <- totalmsgs] of
                [] -> Nothing
                msgs -> Just $ Message.Underlines msgs
        notes = [Just x | Left x <- totalmsgs]
        sections = catMaybes $ underlines_section : notes

    in return $ Message.SimpleDiag Message.Error new_sp Nothing (Just diag_name) sections

    where
        if_span m_sp ty imp msg =
            case m_sp of
                Just sp -> Right $ MsgUnds.Underline sp imp [MsgUnds.Message ty msg]
                Nothing -> Left $ Message.Note msg


instance Message.ToDiagnostic (IRBuildError, IRCtx) where
    to_diagnostic (DuplicateValue name old new, irctx) =
        Reader.runReader
            (duplicate_msg "value" "redecl-val" name old new)
            irctx

    -- TODO:
    -- to_diagnostic (DuplicateLocal fun (Local name old_lvalue _) new_lvalue, irctx) =
        -- duplicate_msg "local" "redecl-local" name (decl_span irctx (fun, old_lvalue)) (decl_span irctx (fun, new_lvalue))

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
-- Lowerable class {{{1
class Lowerable l p where
    ddeclare, ddefine, vdeclare, vdefine :: l -> DSIdx Module -> DSIdx p -> State.State IRBuilder ()
-- build_ir {{{1
build_ir :: AST.LDModule -> State.State IRCtx (DSIdx Module, [IRBuildError])
build_ir mod_ast@(Located mod_sp _) =
    State.state $
        \ irctx ->
            let irb = IRBuilder irctx []
                (mod_idx, IRBuilder irctx' errors) = State.runState lower irb
            in ((mod_idx, errors), irctx')
    where
        lower =
            modify_s' irb_irctx (new_module mod_sp) >>= \ mod_idx ->

            -- TODO:
            -- ddeclare
            -- ddefine
            -- vdeclare
            -- vdefine

            return mod_idx
-- lowering modules {{{1
lower_module :: (AST.LDDecl -> DSIdx Module -> DSIdx Module -> State.State IRBuilder ()) -> Located AST.DModule -> DSIdx Module -> State.State IRBuilder ()
lower_module lower_decl (Located _ (AST.DModule' decls)) mod_idx = mapM_ (\ decl -> lower_decl decl mod_idx mod_idx) decls
