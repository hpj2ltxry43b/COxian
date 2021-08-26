{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR
    ( IRCtx

    , build_ir

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
import StateReader

import Data.Maybe (catMaybes)
import Data.List (find)

import qualified Control.Monad.State as State (State, state, runState, get, put)
import qualified Control.Monad.Reader as Reader (Reader, runReader)

-- IRBuilder {{{1
data IRBuilder = IRBuilder IRCtx [IRBuildError]

irb_irctx :: Lens IRBuilder IRCtx
irb_irctx = Lens (\ (IRBuilder i _) -> i)  (\ (IRBuilder _ e) i -> IRBuilder i e)
irb_errors :: Lens IRBuilder [IRBuildError]
irb_errors = Lens (\ (IRBuilder _ e) -> e) (\ (IRBuilder i _) e -> IRBuilder i e)
-- IRBuildError {{{1
data IRBuildError
    = DuplicateValue String (InternerIdx Value') (InternerIdx Value')
    | DuplicateLocal Function Local LValue
    | Unimplemented String Span
    | NotAType Span (InternerIdx DeclSymbol')
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
            (
                view_r v_interner >>= \ interner ->
                duplicate_msg "value" "redecl-val" name (resolve_interner_idx old interner) (resolve_interner_idx new interner)
            )
            irctx

    to_diagnostic (DuplicateLocal fun (Local name old_lvalue _) new_lvalue, irctx) =
        Reader.runReader
            (
                duplicate_msg "local" "redecl-local" name (fun, old_lvalue) (fun, new_lvalue)
            )
            irctx

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

            lower_module ddeclare mod_ast mod_idx >>
            lower_module ddefine mod_ast mod_idx >>
            lower_module vdeclare mod_ast mod_idx >>
            lower_module vdefine mod_ast mod_idx >>

            return mod_idx
-- helper functions {{{1
(>>=?) :: Monad m => m (Maybe a) -> m b -> (a -> m b) -> m b
(>>=?) m f c = m >>= maybe f c
infixl 1 >>=?

(>>=<>) :: Monad m => m (Either e r) -> (e -> m f) -> (r -> m f) -> m f
(>>=<>) m onleft onright = m >>= either onleft onright
infixl 1 >>=<>

add_error :: IRBuildError -> State.State IRBuilder ()
add_error err = over_s irb_errors (++[err])

resolve_path' :: [String] -> DSIdx Module -> State.State IRCtx (Maybe (InternerIdx DeclSymbol'))
resolve_path' segments root = resolve' segments (upcast_dsidx root)
    where
        resolve' [] start = return (Just start)
        resolve' (current:next) start =
            view_s ds_child_list >>= \ child_list ->
            let m_child = get start current child_list
            in case m_child of
                Just child -> resolve' next child
                Nothing -> return Nothing

resolve_path_d :: AST.LDPath -> DSIdx Module -> State.State IRBuilder (Maybe (InternerIdx DeclSymbol'))
resolve_path_d (Located path_sp (AST.DPath' located_segments)) root =
    let segments = map unlocate located_segments
    in modify_s' irb_irctx (resolve_path' segments root) >>= \case
        Just resolved -> return $ Just resolved
        Nothing ->
            add_error (PathDoesntExist path_sp) >>
            return Nothing

resolve_path_v :: AST.LDPath -> DSIdx Module -> State.State IRBuilder (Maybe (InternerIdx Value'))
resolve_path_v (Located path_sp (AST.DPath' located_segments)) root =
    let segments = map unlocate located_segments
        (first_segments, last_segment) = (init segments, last segments)
    in modify_s' irb_irctx (resolve_path' first_segments root) >>= \case
        Just resolved ->
            view_s (irb_irctx `join_lenses` v_child_list) >>= \ child_list ->
            let m_child = get resolved last_segment child_list
            in case m_child of
                Just child -> return $ Just child
                Nothing ->
                    add_error (PathDoesntExist path_sp) >>
                    return Nothing

        Nothing ->
            add_error (PathDoesntExist path_sp) >>
            return Nothing

resolve_ty :: AST.LDType -> DSIdx Module -> State.State IRBuilder (Maybe (DSIdx Type))
resolve_ty (Located path_sp (AST.DType'Path path)) root =
    resolve_path_d path root >>=? return Nothing $ \ ds ->
    to_state (read_r' irb_irctx (downcast_dsidx ds)) >>= \case
        j@(Just _) -> return j
        Nothing ->
            add_error (NotAType path_sp ds) >>
            return Nothing
-- lowering modules {{{1
lower_module :: (AST.LDDecl -> DSIdx Module -> DSIdx Module -> State.State IRBuilder ()) -> Located AST.DModule -> DSIdx Module -> State.State IRBuilder ()
lower_module lower_decl (Located _ (AST.DModule' decls)) mod_idx = mapM_ (\ decl -> lower_decl decl mod_idx mod_idx) decls
-- lowering declarations {{{1
instance Lowerable AST.LDDecl p where
    ddeclare (Located _ (AST.DDecl'Fun sf)) = ddeclare sf

    ddefine (Located _ (AST.DDecl'Fun sf)) = ddefine sf

    vdeclare (Located _ (AST.DDecl'Fun sf)) = vdeclare sf

    vdefine (Located _ (AST.DDecl'Fun sf)) = vdefine sf
-- lowering functions {{{1
instance Lowerable AST.LSFunDecl p where
    -- functions do not lower to anything during the declaration phases
    ddeclare _ _ _ = return ()
    ddefine _ _ _ = return ()

    vdeclare (Located fun_sp (AST.SFunDecl' mretty (Located _ name) params _)) root parent =
        case mretty of
            Just retty -> resolve_ty retty root
            Nothing -> Just <$> modify_s' irb_irctx (get_ds UnitType)
        >>=? return () $ \ retty' ->
        let make_param (Located sp (AST.DParam'Normal ty_ast _)) =
                resolve_ty ty_ast root >>=? return Nothing $ \ ty ->
                return $ Just (ty, sp)
        in sequence <$> mapM make_param params >>=? return () $ \ param_tys ->
        let fun = new_function retty' param_tys fun_sp
        in modify_s (irb_irctx `join_lenses` function_interner) (get_from_interner fun) >>= \ fun_idx ->
        modify_s' irb_irctx (new_function_pointer fun_idx) >>= \ fptr ->
        upcast_vidx <$> modify_s' irb_irctx (get_v fptr) >>= \ fptr_val ->
        view_s (irb_irctx `join_lenses` v_child_list) >>= \ child_list ->
        case add_noreplace (upcast_dsidx parent) name fptr_val child_list of
            Left other_value ->
                add_error (DuplicateValue name other_value fptr_val) >>
                return ()
            Right added ->
                put_s (irb_irctx `join_lenses` v_child_list) added >>
                return ()

    vdefine (Located _ sf@(AST.SFunDecl' _ (Located _ name) _ _)) root parent =
        get (upcast_dsidx parent) name <$> view_s (irb_irctx `join_lenses` v_child_list) >>=? (return ()) $ \ val ->
        to_state (read_r' irb_irctx $ downcast_vidx val) >>= \case
            -- silently ignore becuase the only way this can happen is if there is another global declaration
            -- that made a value of the same name that is not a function, which should already be reported as a duplicate value error
            Nothing -> return ()

            Just old_fun -> lower_fun_body sf root old_fun
-- lowering function bodies {{{2
data Local = Local String LValue Integer
data FunctionCG = FunctionCG IRBuilder Function Integer [Local]

fcg_irb :: Lens FunctionCG IRBuilder
fcg_irb = Lens (\ (FunctionCG b _ _ _) -> b)  (\ (FunctionCG _ f i l) b -> FunctionCG b f i l)
fcg_fun :: Lens FunctionCG Function
fcg_fun = Lens (\ (FunctionCG _ f _ _) -> f)  (\ (FunctionCG b _ i l) f -> FunctionCG b f i l)
fcg_scope_index :: Lens FunctionCG Integer
fcg_scope_index = Lens (\ (FunctionCG _ _ i _) -> i)  (\ (FunctionCG b f _ l) i -> FunctionCG b f i l)
fcg_locals :: Lens FunctionCG [Local]
fcg_locals = Lens (\ (FunctionCG _ _ _ l) -> l) (\ (FunctionCG b f i _) l -> FunctionCG b f i l)
-- FunctionCG functions {{{3
add_local :: String -> LValue -> State.State FunctionCG (Either Local ())
add_local name lvalue =
    get_local name >>= \case
        Just old -> return $ Left old
        Nothing ->
            view_s fcg_scope_index >>= \ scope_index ->
            let new_local = Local name lvalue scope_index
            in over_s fcg_locals (new_local:) >>
            return (Right ())

get_local :: String -> State.State FunctionCG (Maybe Local)
get_local name = find (\ (Local n _ _) -> n == name) <$> view_s fcg_locals
-- lower function body {{{3
lower_fun_body :: AST.SFunDecl -> DSIdx Module -> VIdx ConstFunctionPointer -> State.State IRBuilder ()
lower_fun_body (AST.SFunDecl' _ _ params body) root fptr_idx =
    to_state (read_r' irb_irctx $ resolve_vidx fptr_idx) >>= \ fptr ->
    let fun_idx = get_function_idx fptr
    in resolve_interner_idx fun_idx <$> view_s (irb_irctx `join_lenses` function_interner) >>= \ fun ->
    (if function_not_defined fun then return $ Just () else return Nothing) >>=? (return ()) $ \ _ ->
    let param_to_local (Located _ (AST.DParam'Normal _ (Located _ param_name))) reg_idx =
            let param_local = LVRegister reg_idx
            in add_local param_name param_local >>= \case
                Right () -> return ()
                Left old_local ->
                    modify_s' fcg_irb (add_error (DuplicateLocal fun old_local param_local)) >>
                    return ()

        lower_action =
            sequence (zipWith param_to_local params (get_param_regs fun)) >>
            lower_body_expr body root

    in State.get >>= \ irb ->
    let fcg = FunctionCG irb fun 0 []
        (_, FunctionCG irb' fun' _ _) = State.runState lower_action fcg
        fun'' = simplify_cfg fun'
    in State.put irb' >>
    -- TODO: replace function in interner
    _
-- lowering things {{{3
lower_body_expr :: AST.LSBlockExpr -> DSIdx Module -> State.State FunctionCG ()
lower_body_expr body root = _
