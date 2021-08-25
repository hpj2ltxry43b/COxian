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

import qualified Control.Monad.State as State (State, state)
import qualified Control.Monad.Reader as Reader (Reader, reader)

-- build_ir {{{1
build_ir :: AST.LDModule -> (Module, IRCtx, [IRBuildError])
build_ir mod_ast@(Located mod_sp _) = (lowered_mod, lowered_irctx, errors)
    where
        apply_stage :: (AST.LDModule -> Module -> ModParent -> IRBuilder -> (ModParent, IRBuilder)) -> (ModParent, IRBuilder) -> (ModParent, IRBuilder)
        apply_stage fun (mod_parent@(ModParent module_), ir_builder) =
            let (module_', ir_builder') = fun mod_ast module_ mod_parent ir_builder
            in (module_', ir_builder')

        initial_cgtup = (ModParent module_, IRBuilder irctx [])
            where
                (module_, irctx) = new_module mod_sp new_irctx
        (ModParent lowered_mod, IRBuilder lowered_irctx errors) =
            apply_stage vdefine .
            apply_stage vdeclare .
            apply_stage ddefine .
            apply_stage ddeclare $
            initial_cgtup
-- IRBuilder {{{1
data IRBuilder = IRBuilder IRCtx [IRBuildError]

get_irctx :: IRBuilder -> IRCtx
get_irctx (IRBuilder c _) = c
-- IRBuildError {{{1
data IRBuildError
    = DuplicateValue String Value Value
    | DuplicateLocal Function Local LValue
    | Unimplemented String Span
    | NotAType Span DeclSymbol
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

-- helper functions {{{1
lower_all_in_list :: Lowerable l p => [l] -> (l -> Module -> p -> IRBuilder -> (p, IRBuilder)) -> Module -> p -> IRBuilder -> (p, IRBuilder)
lower_all_in_list things fun root start irb = foldl' apply_fun (start, irb) things
    where
        apply_fun (p, irb') thing = fun thing root p irb'

lower_all_in_list_s :: Lowerable l p => [l] -> (l -> Module -> p -> IRBuilder -> (p, IRBuilder)) -> Module -> p -> State.State IRBuilder p
lower_all_in_list_s things fun root start = State.state $ lower_all_in_list things fun root start

(>>=?) :: Monad m => m (Maybe a) -> m b -> (a -> m b) -> m b
(>>=?) m f c = m >>= maybe f c
infixl 1 >>=?

(>>=<>) :: Monad m => m (Either e r) -> (e -> m f) -> (r -> m f) -> m f
(>>=<>) m onleft onright = m >>= either onleft onright
infixl 1 >>=<>

add_error :: IRBuildError -> IRBuilder -> IRBuilder
add_error err (IRBuilder irctx errs) = IRBuilder irctx (errs ++ [err])

add_error_s :: IRBuildError -> State.State IRBuilder ()
add_error_s err = State.state $ \ irb -> ((), add_error err irb)

apply_irctx_to_irbuilder :: (IRCtx -> (r, IRCtx)) -> IRBuilder -> (r, IRBuilder)
apply_irctx_to_irbuilder fun (IRBuilder irctx errs) =
    let (r, irctx') = fun irctx
    in (r, IRBuilder irctx' errs)

apply_irctx_to_irbuilder_s :: (IRCtx -> (r, IRCtx)) -> State.State IRBuilder r
apply_irctx_to_irbuilder_s fun = State.state $ apply_irctx_to_irbuilder fun

get_s :: Parent p c i => i -> p -> State.State IRBuilder (Maybe c)
get_s ind parent = State.state $ \ irb ->
    let (IRBuilder irctx _) = irb
    in (get ind (parent, irctx), irb)

add_replace_s :: Parent p c i => i -> c -> p -> State.State IRBuilder p
add_replace_s ind child parent = State.state $ \ irb ->
    let (IRBuilder irctx errors) = irb
        (parent', irctx') = add_replace ind child (parent, irctx)
        irb' = IRBuilder irctx' errors
    in (parent', irb')

add_noreplace_s :: Parent p c i => i -> c -> p -> State.State IRBuilder (Either c p)
add_noreplace_s ind child parent = State.state $ \ irb ->
    let (IRBuilder irctx errors) = irb
    in case add_noreplace ind child (parent, irctx) of
        Left old -> (Left old, irb)
        Right (parent', irctx') -> (Right parent', IRBuilder irctx' errors)
-- path resultion, type resolution, type interning {{{1
resolve_path_d :: AST.LDPath -> Module -> IRBuilder -> (Maybe (DeclSymbol, DSIRId DeclSymbol), IRBuilder)
resolve_path_d (Located path_sp (AST.DPath' located_segments)) root ir_builder@(IRBuilder irctx _) =
    case m_dsid of
        Just dsid -> (Just (resolve_dsid irctx root dsid, dsid), ir_builder)
        Nothing -> (Nothing, add_error (PathDoesntExist path_sp) ir_builder)
    where
        segments = map unlocate located_segments
        m_dsid = new_dsid irctx root segments :: Maybe (DSIRId DeclSymbol)

resolve_path_v :: AST.LDPath -> Module -> IRBuilder -> (Maybe (Value, VIRId Value), IRBuilder)
resolve_path_v (Located path_sp (AST.DPath' located_segments)) root ir_builder@(IRBuilder irctx _) =
    case m_vid of
        Just vid -> (Just (resolve_vid irctx root vid, vid), ir_builder)
        Nothing -> (Nothing, add_error (PathDoesntExist path_sp) ir_builder)
    where
        segments = map unlocate located_segments
        m_vid = new_vid irctx root segments :: Maybe (VIRId Value)

resolve_ty_s :: AST.LDType -> Module -> State.State IRBuilder (Maybe (InternerIdx Type))

resolve_ty_s (Located path_sp (AST.DType'Path path)) root =
    State.state (resolve_path_d path root) >>=? return Nothing $ \ (ds, _) ->
    case ds_cast ds :: Maybe (InternerIdx Type) of
        j@(Just _) -> return j
        Nothing ->
            add_error_s (NotAType path_sp ds) >>
            return Nothing

{-
resolve_ty_s (Located _ (AST.DType'Pointer pointee)) root =
    resolve_ty_s pointee root >>=? return Nothing $ \ pointee_idx ->
    let pointer_ty = PointerType Map.empty pointee_idx
    in Just <$> get_ty_s pointer_ty

resolve_ty_s (Located sp AST.DType'This) _ =
    add_error_s (Unimplemented "'this' types" sp) >> -- TODO
    return Nothing
-}

get_ty_s :: Type -> State.State IRBuilder (InternerIdx Type)
get_ty_s ty = State.state $ \ (IRBuilder ctx errs) ->
    let (idx, ctx') = get_ty_irctx ty ctx
    in (idx, IRBuilder ctx' errs)
-- Lowerable class {{{1
class Lowerable l p where
    ddeclare, ddefine, vdeclare, vdefine :: l -> Module -> p -> IRBuilder -> (p, IRBuilder)
-- lowering modules {{{1
newtype ModParent = ModParent Module
instance Parent ModParent Module () where
    get_child_map (ModParent mp, _) = Map.singleton () mp
    add _ child (ModParent prev, irctx) = (Just prev, (ModParent child, irctx))

lower_module :: Parent a Module () => (AST.LDDecl -> Module -> Module -> IRBuilder -> (Module, IRBuilder)) -> Located AST.DModule -> Module -> a -> IRBuilder -> (a, IRBuilder)
lower_module fun (Located _ (AST.DModule' decls)) root parent = State.runState $
    get_s () parent >>= \ (Just (module_ :: Module)) ->
    lower_all_in_list_s decls fun root module_ >>= \ added_module ->
    add_replace_s () added_module parent >>= \ added_parent ->
    return added_parent

instance Parent p Module () => Lowerable AST.LDModule p where
    ddeclare = lower_module ddeclare
    ddefine = lower_module ddefine
    vdeclare = lower_module vdeclare
    vdefine = lower_module vdefine
-- lowering functions {{{1
instance Parent p Value String => Lowerable AST.LSFunDecl p where
    -- functions do not lower to anything during the declaration phases
    ddeclare _ _ = (,)
    ddefine _ _ = (,)

    vdeclare (Located fun_sp (AST.SFunDecl' mretty (Located _ name) params _)) root parent = State.runState $
        case mretty of
            Just retty -> resolve_ty_s retty root
            Nothing -> Just <$> get_ty_s (UnitType Map.empty)
         >>=? return parent $ \ retty' ->
        let make_param :: AST.LDParam -> State.State IRBuilder (Maybe (InternerIdx Type, Span))
            make_param (Located sp (AST.DParam'Normal ty_ast _)) =
                resolve_ty_s ty_ast root >>=? return Nothing $ \ ty ->
                return $ Just (ty, sp)
        in sequence <$> mapM make_param params >>=? return parent $ \ param_tys ->
        let fun = new_function retty' param_tys fun_sp
        in apply_irctx_to_irbuilder_s (add_function fun) >>= \ fun_idx ->
        apply_irctx_to_irbuilder_s (new_function_pointer fun_idx) >>= \ fptr ->
        let fptr_val = Value fptr
        in add_noreplace_s name fptr_val parent >>= \case
            Left other_value ->
                add_error_s (DuplicateValue name other_value fptr_val) >>
                return parent
            Right added -> return added

    vdefine (Located _ sf@(AST.SFunDecl' _ (Located _ name) _ _)) root parent = State.runState $
        get_s name parent >>= \ m_val ->
        let m_fun = m_val >>= value_cast :: Maybe ConstFunctionPointer
        in case m_fun of
            -- silently ignore becuase the only way this can happen is if there is another global declaration
            -- that made a value of the same name that is not a function, which should already be reported as a duplicate value error
            Nothing -> return parent

            Just old_fun -> lower_fun_body sf root old_fun parent
-- lowering function bodies {{{2
data Local = Local String LValue Integer
data FunctionCG = FunctionCG Integer [Local]
-- FunctionCG functions {{{3
add_local :: String -> LValue -> FunctionCG -> Either Local FunctionCG
add_local name lvalue fcg@(FunctionCG scope_idx locals) =
    case get_local name fcg of
        Just old -> Left old
        Nothing -> Right $ FunctionCG scope_idx (Local name lvalue scope_idx : locals)

add_local_s :: String -> LValue -> State.State FunctionCG (Either Local ())
add_local_s name lvalue = State.state $ \ fcg ->
    let elfcg = add_local name lvalue fcg
    in case elfcg of
        Right fcg' -> (Right (), fcg')
        Left old -> (Left old, fcg)

get_local :: String -> FunctionCG -> Maybe Local
get_local name (FunctionCG _ locals) = find (\ (Local n _ _) -> n == name) locals
-- function cfg modification states {{{3
add_instruction_s :: Instruction -> BlockIdx -> State.State (IRBuilder, FunctionCG, Function) InstructionIdx
add_instruction_s instr block_idx = apply_fun_to_funcgtup_s (State.state $ add_instruction instr block_idx)

add_br_s :: Br -> BlockIdx -> State.State (IRBuilder, FunctionCG, Function) ()
add_br_s br block_idx = apply_fun_to_funcgtup_s (State.state $ (,) () . add_br br block_idx)

add_basic_block_s :: String -> State.State (IRBuilder, FunctionCG, Function) BlockIdx
add_basic_block_s name = apply_fun_to_funcgtup_s (State.state $ add_basic_block name)
-- triplecgtup applications helpers {{{3
apply_irb_to_funcgtup_s :: State.State IRBuilder r -> State.State (IRBuilder, FunctionCG, Function) r
apply_irb_to_funcgtup_s st = State.state $ \ (irb, fcg, fun) ->
    let (r, irb') = State.runState st irb
    in (r, (irb', fcg, fun))

apply_fcg_to_funcgtup_s :: State.State FunctionCG r -> State.State (IRBuilder, FunctionCG, Function) r
apply_fcg_to_funcgtup_s st = State.state $ \ (irb, fcg, fun) ->
    let (r, fcg') = State.runState st fcg
    in (r, (irb, fcg', fun))

apply_fun_to_funcgtup_s :: State.State Function r -> State.State (IRBuilder, FunctionCG, Function) r
apply_fun_to_funcgtup_s st = State.state $ \ (irb, fcg, fun) ->
    let (r, fun') = State.runState st fun
    in (r, (irb, fcg, fun'))
-- report_type_error {{{3
report_type_error :: TypeError -> State.State (IRBuilder, FunctionCG, Function) ()
report_type_error = apply_irb_to_funcgtup_s . add_error_s . TypeError
-- making instructions {{{3
make_instr_s :: (IRCtx -> Function -> Module -> a) -> (a -> (Either TypeError r, IRCtx)) -> Module -> State.State (IRBuilder, FunctionCG, Function) (Either TypeError r)
make_instr_s make_fun appl root = State.state $
    \ (IRBuilder irctx errs, fcg, fun) ->
    let (instr, irctx') = appl $ make_fun irctx fun root
    in (instr, (IRBuilder irctx' errs, fcg, fun))

make_copy_s :: Module -> Located LValue -> String -> Located FValue -> String -> State.State (IRBuilder, FunctionCG, Function) (Either TypeError Instruction)
make_copy_s root lv lvn fv fvn = make_instr_s make_copy (\ a -> a lv lvn fv fvn) root

make_call_s :: Module -> FValue -> [FValue] -> State.State (IRBuilder, FunctionCG, Function) (Either TypeError Instruction)
make_call_s root fv args = make_instr_s make_call (\ a -> a fv args) root

-- make_addrof_s :: Module -> LValue -> State.State (IRBuilder, FunctionCG, Function) (Either TypeError Instruction)
-- make_addrof_s root lv = make_instr_s make_addrof (\ a -> a lv) root

-- make_derefptr_s :: Module -> FValue -> State.State (IRBuilder, FunctionCG, Function) (Either TypeError Instruction)
-- make_derefptr_s root fv = make_instr_s make_derefptr ($fv) root

make_br_cond_s :: Module -> Located FValue -> BlockIdx -> BlockIdx -> State.State (IRBuilder, FunctionCG, Function) (Either TypeError Br)
make_br_cond_s root fv t f = make_instr_s make_br_cond (\ a -> a fv t f) root
-- lower function body {{{3
lower_fun_body :: Parent p Value String => AST.SFunDecl -> Module -> ConstFunctionPointer -> p -> State.State IRBuilder p
lower_fun_body (AST.SFunDecl' _ _ params body) root fptr parent =
    State.get >>= \ (IRBuilder irctx _) ->
    let fun = get_fptr_pointee irctx fptr
        fun_idx = get_function_idx fptr
        function_valid = if function_not_defined fun then Just () else Nothing
    in return function_valid >>=? return parent $ \ _ ->
    let param_to_local (Located _ (AST.DParam'Normal _ (Located _ param_name)), reg_idx) function_cg =
            let m_function_cg' = add_local param_name (LVRegister reg_idx) function_cg
            in case m_function_cg' of
                Right function_cg' -> return function_cg'
                Left old_local ->
                    add_error_s (DuplicateLocal fun old_local (LVRegister reg_idx)) >>
                    return function_cg
        add_locals_for_params = zipWith (curry param_to_local) params (get_param_regs fun)

        start_function_cg = return $ FunctionCG 0 []

    in foldl' (>>=) start_function_cg add_locals_for_params >>= \ function_cg ->

    State.get >>= \ ir_builder ->
    let (ir_builder'@(IRBuilder _ errs) , _, fun') = State.execState (lower_body_expr body root) (ir_builder, function_cg, fun)
        fun'' = simplify_cfg fun'
        ir_builder'' = IRBuilder (replace_function fun'' fun_idx (get_irctx ir_builder')) errs
    in State.put ir_builder'' >>
    return parent
-- lowering things {{{3
data BlockGroup
    = BlockGroup
      { bg_start :: BlockIdx
      , bg_cont :: Maybe BlockIdx
      }
data BlockGroupCont
    = BlockGroupCont
      { bgc_start :: BlockIdx
      , bgc_cont :: BlockIdx
      }

bg_bgc :: Monad m => BlockGroup -> m b -> (BlockGroupCont -> m b) -> m b
bg_bgc (BlockGroup _ Nothing) on_no_cont _ = on_no_cont
bg_bgc (BlockGroup s (Just c)) _ on_cont = on_cont $ BlockGroupCont s c
infixl 1 `bg_bgc`

lower_body_expr :: AST.LSBlockExpr -> Module -> State.State (IRBuilder, FunctionCG, Function) ()
lower_body_expr body root =
    let fail_block =
            add_basic_block_s "failed_body_lowering" >>= \ bl ->
            return (bl, bl)

        link_block_to_function (start, end) =
            State.get >>= \ (_,  _, fun) ->

            add_br_s (make_br_goto start) (get_entry_block fun) >>
            add_br_s (make_br_goto $ get_exit_block fun) end

    in lower_block_expr body root >>=? (fail_block >>= link_block_to_function) $ \ (expr_blocks', res) ->
    expr_blocks' `bg_bgc` return () $ \ expr_blocks ->

    State.get >>= \ (_,  _, fun) ->

    add_basic_block_s "end_block" >>= \ end_block ->
    add_br_s (make_br_goto end_block) (bgc_cont expr_blocks) >>
    make_copy_s root (Located (get_span fun) (LVRegister $ get_ret_reg fun)) "function's return type" res "return value's type" >>=<> ((>> (fail_block >>= link_block_to_function)) . report_type_error) $ \ copy_instr ->
    add_instruction_s copy_instr end_block >>

    link_block_to_function (bgc_start expr_blocks, end_block)

lower_expr :: AST.LDExpr -> Module -> State.State (IRBuilder, FunctionCG, Function) (Maybe (BlockGroup, Located FValue))

lower_expr (Located _ (AST.DExpr'Block block)) root = lower_block_expr block root

lower_expr (Located sp (AST.DExpr'If cond trueb@(Located truebsp _) m_falseb)) root =
    lower_expr cond root >>=? return Nothing $ \ (cond_ir', cond_val) ->
    lower_expr trueb root >>=? return Nothing $ \ (trueb_ir', trueb_val) ->

    cond_ir' `bg_bgc` return Nothing $ \ cond_ir ->
    trueb_ir' `bg_bgc` return Nothing $ \ trueb_ir ->

    State.get >>= \ (irb, _, fun) ->
    let irctx = get_irctx irb
    in apply_fun_to_funcgtup_s (State.state $ add_register (type_of irctx (root, fun, unlocate trueb_val)) sp) >>= \ ret_reg ->

    add_basic_block_s "if_after" >>= \ end_block ->

    let block_and_ret_reg name ir val val_name =
            make_copy_s root (Located truebsp (LVRegister ret_reg)) "true block's type" val val_name >>=<> (return . Left) $ \ copy_instr ->
            add_basic_block_s ("if_put_" ++ name ++ "_value_to_ret_reg") >>= \ block ->

            add_br_s (make_br_goto block) (bgc_cont ir) >>

            add_instruction_s copy_instr block >>
            add_br_s (make_br_goto end_block) block >>

            return (Right $ BlockGroupCont (bgc_start ir) block)

    in block_and_ret_reg "true" trueb_ir trueb_val (error "true block result doesn't match type of result register") >>=<> ((>>return Nothing) . report_type_error) $ \ trueb_ir'' ->
    let cond_br = make_br_cond_s root cond_val (bgc_start trueb_ir'')
    in (
        case m_falseb of
            Just falseb ->
                lower_expr falseb root >>=? return Nothing $ \ (falseb_ir', falseb_val) ->
                falseb_ir' `bg_bgc` return Nothing $ \ falseb_ir ->

                block_and_ret_reg "false" falseb_ir falseb_val "false branch's result type" >>=<> ((>>return Nothing) . report_type_error) $ \ falseb_ir'' ->
                cond_br (bgc_start falseb_ir'') >>=<> ((>>return Nothing) . report_type_error) $ \ br ->

                add_br_s br (bgc_cont cond_ir) >>

                return (Just $ BlockGroup (bgc_start cond_ir) (Just end_block))

            Nothing ->
                cond_br end_block >>=<> ((>>return Nothing) . report_type_error) $ \ br ->

                add_br_s br (bgc_cont cond_ir) >>

                return (Just $ BlockGroup (bgc_start cond_ir) (Just end_block))

    ) >>=? return Nothing $ \ blocks ->
    return $ Just (blocks, Located sp $ FVNLVRegister ret_reg)

lower_expr (Located sp (AST.DExpr'While cond body)) root =
    lower_expr cond root >>=? return Nothing $ \ (cond_ir', cond_val) ->
    lower_expr body root >>=? return Nothing $ \ (body_ir', _) ->

    cond_ir' `bg_bgc` return Nothing $ \ cond_ir ->
    body_ir' `bg_bgc` return Nothing $ \ body_ir ->

    add_basic_block_s "while_after" >>= \ while_after ->

    make_br_cond_s root cond_val (bgc_start body_ir) while_after >>=<> ((>>return Nothing) . report_type_error) $ \ cond_br ->
    add_br_s cond_br (bgc_cont cond_ir) >>

    add_br_s (make_br_goto while_after) (bgc_cont body_ir) >>

    return (Just (BlockGroup (bgc_start cond_ir) (Just while_after), Located sp FVUnit))

lower_expr (Located sp (AST.DExpr'Assign target@(Located target_sp _) (Located op_sp AST.Equal) expr)) root =
    lower_expr target root >>=? return Nothing $ \ (target_ir', target_val) ->
    lower_expr expr root >>=? return Nothing $ \ (expr_ir', expr_val) ->

    target_ir' `bg_bgc` return Nothing $ \ target_ir ->
    expr_ir' `bg_bgc` return Nothing $ \ expr_ir ->

    case target_val of
        (Located lvsp (FVLValue lv)) ->
            make_copy_s root (Located lvsp lv) "assignment target's type" expr_val "expression's type" >>=<> ((>>return Nothing) . report_type_error) $ \ copy_instr ->
            add_basic_block_s "assign_block" >>= \ assign_block ->
            add_instruction_s copy_instr assign_block >>

            add_br_s (make_br_goto (bgc_start expr_ir)) (bgc_cont target_ir) >>
            add_br_s (make_br_goto assign_block) (bgc_cont expr_ir) >>

            return (Just (BlockGroup (bgc_start target_ir) (Just assign_block), Located sp FVUnit))

        _ ->
            apply_irb_to_funcgtup_s (add_error_s $ InvalidAssign target_sp op_sp) >>
            return Nothing

lower_expr (Located sp (AST.DExpr'ShortCircuit _ _ _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "short-circuiting binary expressions" sp) >> -- TODO
    return Nothing

lower_expr (Located sp (AST.DExpr'Binary _ _ _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "binary expressions" sp) >> -- TODO
    return Nothing

lower_expr (Located sp (AST.DExpr'Cast _ _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "cast expression" sp) >> -- TODO
    return Nothing

lower_expr (Located sp (AST.DExpr'Unary _ _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "unary expressions" sp) >> -- TODO
    return Nothing

{-
lower_expr (Located sp (AST.DExpr'Ref expr)) root =
    lower_expr expr root >>=? return Nothing $ \ (expr_ir, expr_val) ->

    case expr_val of
        Located _ (FVLValue expr_lv) ->
            make_addrof_s root expr_lv >>=<> ((>>return Nothing) . report_type_error) $ \ ref_instr ->

            add_basic_block_s "ref_block" >>= \ ref_block ->
            add_instruction_s ref_instr ref_block >>= \ instr_idx ->

            add_br_s (make_br_goto ref_block) (bgc_cont expr_ir) >>

            return (Just ((bgc_start expr_ir, ref_block), Located sp $ FVInstruction instr_idx))

        _ ->
            apply_irb_to_funcgtup_s (add_error_s $ AddrofNotLValue sp) >>
            return Nothing

lower_expr (Located sp (AST.DExpr'Deref _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "dereference expressions" sp) >> -- TODO
    return Nothing
-}

lower_expr (Located sp (AST.DExpr'Call _ _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "call expressions" sp) >> -- TODO
    return Nothing

{-
lower_expr (Located sp (AST.DExpr'Field _ _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "field access expressions" sp) >> -- TODO
    return (make_halfway_block _ [] Nothing, Nothing)

lower_expr (Located sp (AST.DExpr'Method _ _ _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "method call expressions" sp) >> -- TODO
    return (make_halfway_block _ [] Nothing, Nothing)
-}

lower_expr (Located sp (AST.DExpr'Int i)) _ =
    apply_irb_to_funcgtup_s (get_ty_s (IntType Map.empty 32 Unsigned)) >>= \ ty ->
    add_basic_block_s "literal_int_expr" >>= \ block ->
    return (Just (BlockGroup block (Just block), Located sp $ FVConstInt i ty))

lower_expr (Located sp (AST.DExpr'Float d)) _ =
    apply_irb_to_funcgtup_s (get_ty_s (FloatType Map.empty 32)) >>= \ ty ->
    add_basic_block_s "literal_float_expr" >>= \ block ->
    return (Just (BlockGroup block (Just block), Located sp $ FVConstFloat d ty))

lower_expr (Located sp (AST.DExpr'Bool b)) _ =
    add_basic_block_s "literal_bool_expr" >>= \ block ->
    return $ Just (BlockGroup block (Just block), Located sp $ FVConstBool b)

lower_expr (Located sp (AST.DExpr'Char c)) _ =
    add_basic_block_s "literal_char_expr" >>= \ block ->
    return $ Just (BlockGroup block (Just block), Located sp $ FVConstChar c)

lower_expr (Located sp (AST.DExpr'String _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "string literal expressions" sp) >> -- TODO
    return Nothing

{-
lower_expr (Located sp AST.DExpr'This) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "'this' expressions" sp) >> -- TODO
    return (make_halfway_block _ [] Nothing, Nothing)
-}

lower_expr (Located sp (AST.DExpr'Path path)) root =
    (case path of
        (Located _ (AST.DPath' [Located _ iden])) ->
            State.get >>= \ (_, fcg, _) ->
            let lvalue =
                    case get_local iden fcg of
                        Just (Local _ l _) -> Just l
                        Nothing -> Nothing
            in return $ FVLValue <$> lvalue

        _ -> return Nothing
    ) >>= \ m_res ->
    add_basic_block_s "resolve_path_expr" >>= \ block ->
    case m_res of
        Just reg_fv -> return $ Just (BlockGroup block (Just block), Located sp reg_fv)
        Nothing ->
            apply_irb_to_funcgtup_s (State.state $ resolve_path_v path root) >>=? return Nothing $ \ (_, vid) ->
            return $ Just (BlockGroup block (Just block), Located sp $ FVGlobalValue vid)

lower_expr (Located sp (AST.DExpr'Ret expr)) root =
    lower_expr expr root >>=? return Nothing $ \ (expr_ir', expr_val) ->
    expr_ir' `bg_bgc` return Nothing $ \ expr_ir ->

    State.get >>= \ (_, _, fun) ->

    add_basic_block_s "put_ret_val" >>= \ put_block ->

    add_br_s (make_br_goto put_block) (bgc_cont expr_ir) >>

    make_copy_s root (Located (get_span fun) (LVRegister $ get_ret_reg fun)) "function's return type" expr_val "return value's type" >>=<> ((>>return Nothing) . report_type_error) $ \ copy_instr ->
    add_instruction_s copy_instr put_block >>
    add_br_s (make_br_goto $ get_exit_block fun) put_block >>

    return (Just (BlockGroup (bgc_start expr_ir) Nothing, Located sp FVUnit))

lower_block_expr :: AST.LSBlockExpr -> Module -> State.State (IRBuilder, FunctionCG, Function) (Maybe (BlockGroup, Located FValue))
lower_block_expr (Located blocksp (AST.SBlockExpr' stmts)) root =
    let safe_last [] = Nothing
        safe_last x = Just $ last x

        safe_init [] = []
        safe_init l = init l

        (stmts', m_ret_expr) = case safe_last stmts of
            Just (Located _ (AST.DStmt'Expr ret)) -> (safe_init stmts, Just ret)
            _ -> (stmts, Nothing)

    in mapM (`lower_stmt` root) stmts' >>= \ stmts_m_ir ->
    let stmts_ir = catMaybes stmts_m_ir
    in

    (case m_ret_expr of
        Just ret_expr ->
            lower_expr ret_expr root >>=? return (Nothing, Nothing) $ \ (ir, val) ->
            return (Just ir, Just val)
        Nothing -> return (Nothing, Nothing)
    ) >>= \ (m_ret_ir, m_ret_val) ->

    let total_irs =
            case m_ret_ir of
                Just ret_ir -> stmts_ir ++ [ret_ir]
                Nothing -> stmts_ir

        set_brs [] = return []
        set_brs [x] = return [x]
        set_brs (current : more@(next:_)) =
            case bg_cont current of
                Just cont ->
                    add_br_s (make_br_goto (bg_start next)) cont >>
                    set_brs more >>= \ more_set ->
                    return (current : more_set)

                Nothing ->
                    return [current]

        res_val = case m_ret_val of
            Just ret_val -> ret_val
            Nothing -> Located blocksp FVUnit

    in set_brs total_irs >>= \case
        [] ->
            add_basic_block_s "empty_block_expr" >>= \ b ->
            return (Just (BlockGroup b (Just b), res_val))
        [single] -> return $ Just (single, res_val)
        first_block:more ->
            let last_block = last more
            in return (Just (BlockGroup (bg_start first_block) (bg_cont last_block), res_val))

lower_stmt :: AST.LDStmt -> Module -> State.State (IRBuilder, FunctionCG, Function) (Maybe BlockGroup)
lower_stmt (Located _ (AST.DStmt'Expr ex)) root = (fst <$>) <$> lower_expr ex root

lower_stmt (Located _ (AST.DStmt'Var ty (Located name_sp name) m_init)) root =
    apply_irb_to_funcgtup_s (resolve_ty_s ty root) >>=? return Nothing $ \ var_ty_idx ->
    apply_fun_to_funcgtup_s (State.state $ add_register var_ty_idx name_sp) >>= \ reg_idx ->
    apply_fcg_to_funcgtup_s (add_local_s name (LVRegister reg_idx)) >>= \ m_old ->
    (case m_old of
        Left old ->
            State.get >>= \ (_, _, fun) ->
            apply_irb_to_funcgtup_s (add_error_s $ DuplicateLocal fun old (LVRegister reg_idx)) >>
            return Nothing

        Right () -> return $ Just ()
    ) >>=? return Nothing $ \ _ ->
    case m_init of
        Nothing -> add_basic_block_s "new_var" >>= \ block -> return (Just $ BlockGroup block (Just block))
        Just init_expr ->
            lower_expr init_expr root >>=? return Nothing $ \ (init_expr_ir', init_expr_val) ->
            init_expr_ir' `bg_bgc` return Nothing $ \ init_expr_ir ->

            add_basic_block_s "init_var" >>= \ init_block ->
            make_copy_s root (Located name_sp $ LVRegister reg_idx) "variable's type" init_expr_val "initializer's type" >>=<> ((>>return Nothing) . report_type_error) $ \ copy_instr ->
            add_instruction_s copy_instr init_block >>

            add_br_s (make_br_goto init_block) (bgc_cont init_expr_ir) >>

            return (Just $ BlockGroup (bgc_start init_expr_ir) (Just init_block))

-- lowering declarations {{{1
instance Parent p Value String => Lowerable AST.LDDecl p where
    ddeclare (Located _ (AST.DDecl'Fun sf)) root parent ir_builder = ddeclare sf root parent ir_builder
    {-
    ddeclare (Located sp (AST.DDecl'Impl _ _)) _ parent ir_builder =
        let warn = Unimplemented "'impl' blocks" sp -- TODO
        in (parent, add_error warn ir_builder)
    -}

    ddefine (Located _ (AST.DDecl'Fun sf)) root = ddefine sf root
    -- ddefine (Located _ (AST.DDecl'Impl _ _)) _ = (,)

    vdeclare (Located _ (AST.DDecl'Fun sf)) root = vdeclare sf root
    -- vdeclare (Located _ (AST.DDecl'Impl _ _)) _ = (,)

    vdefine (Located _ (AST.DDecl'Fun sf)) root = vdefine sf root
    -- vdefine (Located _ (AST.DDecl'Impl _ _)) _ = (,)
