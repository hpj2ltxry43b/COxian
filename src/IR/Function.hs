{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Function
    ( Function
    , BasicBlock
    , Register
    , FValue
        ( FVGlobalValue
        , FVNLVRegister
        , FVLValue
        , FVInstruction
        )
    , LValue(..)
    , Instruction
    , Br

    , BlockIdx
    , InstructionIdx
    , RegisterIdx

    , TypeError
    , make_copy
    , make_call
    , make_br_ret
    , make_br_goto
    , make_br_cond

    , print_fun

    , new_function

    , get_entry_block
    , get_exit_block
    , get_ret_reg
    , get_param_regs
    , get_ret_type
    , get_param_types
    , get_register
    , get_span

    , function_not_defined

    , add_register
    , add_basic_block
    , add_instruction
    , add_br

    , simplify_cfg
    ) where

import StateAndLens
import StateReader

import IR.Type

import IR.Module

import IR.IRCtx

import IR.DeclSpan
import IR.Typed

import Location

import Interner

import qualified Message
import qualified Message.Underlines as MsgUnds

import Data.List (intercalate, findIndex)
import Data.Maybe (mapMaybe, fromJust)

import qualified Control.Monad.State as State (State)
import qualified Control.Monad.Reader as Reader (Reader, runReader)

data Function
    = Function
      { get_blocks :: [BasicBlock]
      , get_entry_block :: BlockIdx
      , get_exit_block :: BlockIdx

      , get_registers :: [Register]
      , get_ret_reg :: RegisterIdx
      , get_param_regs :: [RegisterIdx]

      , get_ret_type :: DSIdx Type
      , get_param_types :: [DSIdx Type]

      , get_instruction_pool :: [Instruction]

      , get_span :: Span
      }
      deriving Eq
newtype BlockIdx = BlockIdx Int deriving Eq
newtype InstructionIdx = InstructionIdx Int deriving Eq
newtype RegisterIdx = RegisterIdx Int deriving Eq

data BasicBlock = BasicBlock String [InstructionIdx] (Maybe Br) deriving Eq
data Register = Register (DSIdx Type) {- Mutability -} Span deriving Eq
data Instruction
    = Copy LValue FValue (DSIdx Type)
    | Call FValue [FValue]
    deriving Eq

data LValue
    = LVRegister RegisterIdx
    deriving Eq
data FValue
    = FVGlobalValue (InternerIdx Value')
    | FVNLVRegister RegisterIdx
    | FVLValue LValue
    | FVConstInt Integer (DSIdx Type)
    | FVConstFloat Double (DSIdx Type)
    | FVConstBool Bool (DSIdx Type)
    | FVConstChar Char (DSIdx Type)
    | FVUnit (DSIdx Type)
    | FVInstruction InstructionIdx
    deriving Eq

data Br
    = BrRet
    | BrGoto BlockIdx
    | BrCond FValue BlockIdx BlockIdx
    deriving Eq

instance DeclSpan IRCtx Register where
    decl_span (Register _ sp) = return $ Just sp
instance Typed IRCtx (DSIdx Type) Register where
    type_of (Register ty _) = return $ ty

instance DeclSpan IRCtx Function where
    decl_span f = return $ Just $ get_span f

instance DeclSpan IRCtx (Function, LValue) where
    decl_span (f, LVRegister reg) = decl_span $ get_register f reg

instance Typed IRCtx (DSIdx Type) (Function, LValue) where
    type_of (f, LVRegister reg) = type_of $ get_register f reg

instance Typed IRCtx (DSIdx Type) (Function, FValue) where
    type_of (_, FVGlobalValue vidx) = view_r v_interner >>= type_of . resolve_interner_idx vidx
    type_of (fun, FVNLVRegister regidx) = type_of $ get_register fun regidx
    type_of (fun, FVLValue lv) = type_of (fun, lv)
    type_of (_, FVConstInt _ ty) = return ty
    type_of (_, FVConstFloat _ ty) = return ty
    type_of (_, FVConstBool _ ty) = return ty
    type_of (_, FVConstChar _ ty) = return ty
    type_of (_, FVUnit ty) = return ty
    type_of (fun, FVInstruction idx) = type_of $ get_instruction fun idx

instance Typed IRCtx (DSIdx Type) Instruction where
    type_of (Copy _ _ ty) = return ty

new_function :: DSIdx Type -> [(DSIdx Type, Span)] -> Span -> Function
new_function ret_type param_tys sp =
    let param_regs = map (uncurry Register) param_tys
        param_reg_idxs = map RegisterIdx $ take (length param_tys) [1..]

        registers = Register ret_type sp : param_regs

        blocks =
            [ BasicBlock "entry" [] Nothing
            , BasicBlock "exit" [] (Just BrRet)
            ]

        param_tys' = map fst param_tys

    in Function blocks (BlockIdx 0) (BlockIdx 1) registers (RegisterIdx 0) param_reg_idxs ret_type param_tys' [] sp

add_register :: DSIdx Type -> Span -> Function -> (RegisterIdx, Function)
add_register tyidx sp fun = (reg_idx, fun')
    where
        reg = Register tyidx sp

        registers = get_registers fun
        fun' = fun
               { get_registers = registers ++ [reg]
               }
        reg_idx = RegisterIdx $ length registers

get_register :: Function -> RegisterIdx -> Register
get_register fun (RegisterIdx idx) = get_registers fun !! idx

get_instruction :: Function -> InstructionIdx -> Instruction
get_instruction (Function _ _ _ _ _ _ _ _ instr_pool _) (InstructionIdx iidx) = instr_pool !! iidx

function_not_defined :: Function -> Bool
function_not_defined = (2==) . length . get_blocks -- a function starts out with 2 blocks, and it only goes up from there; blocks cannot be removed

-- making instructions and branches for typechecking {{{1
-- TypeError datatype {{{2
data TypeError = TypeError TypeErrorClause [TypeErrorClause]
data TypeErrorClause
    = ThingIs String Span (DSIdx Type) Reason
    | ThingShouldBe String Span (DSIdx Type) Reason
data Reason = Because String | NoReason

instance Message.ToDiagnostic (TypeError, IRCtx) where
    to_diagnostic (TypeError main_clause clauses, irctx) = Reader.runReader diag irctx
        where
            diag = 
                sequence (clause_to_underline True main_clause : map (clause_to_underline False) clauses) >>= \ underlines ->
                return (Message.SimpleDiag Message.Error (Just main_span) Nothing Nothing
                    [ Message.Underlines underlines
                    ]
                )

            get_clause_span (ThingIs _ sp _ _) = sp
            get_clause_span (ThingShouldBe _ sp _ _) = sp

            main_span = get_clause_span main_clause

            clause_to_underline is_main clause =
                (case clause of
                    ThingIs thing _ ty reason ->
                        resolve_dsidx ty >>=
                        stringify_ty >>= \ strd_ty ->
                        return $ "the " ++ thing ++ " is '" ++ strd_ty ++ "'" ++ str_reason reason

                    ThingShouldBe thing _ ty reason ->
                        resolve_dsidx ty >>=
                        stringify_ty >>= \ strd_ty ->
                        return $ "the " ++ thing ++ " should be '" ++ strd_ty++ "'" ++ str_reason reason
                ) >>= \ msg ->
                return (MsgUnds.Underline thingsp msg_imp [MsgUnds.Message msg_ty msg])
                where
                    msg_imp
                        | is_main = MsgUnds.Primary
                        | otherwise = MsgUnds.Secondary
                    msg_ty
                        | is_main = MsgUnds.Error
                        | otherwise = MsgUnds.Note

                    thingsp = get_clause_span clause

            str_reason (Because reason) = " because " ++ reason
            str_reason NoReason = ""
-- instructions {{{2
-- TODO: allow caller to supply their own type error
make_copy :: Function -> Located LValue -> String -> Located FValue -> String -> State.State IRCtx (Either TypeError Instruction)
make_copy fun (Located lvsp lv) lv_name (Located fvsp fv) fv_name =
    to_state (type_of (fun, lv)) >>= \ lvty ->
    to_state (type_of (fun, fv)) >>= \ fvty ->
    if lvty == fvty
         then
            get_ds UnitType >>= \ unit_ty ->
             return $ Right $ Copy lv fv unit_ty
         else return $ Left $ TypeError
                     ( ThingIs fv_name fvsp fvty NoReason )
                     [ ThingIs lv_name lvsp lvty NoReason
                     ]
make_call :: Function -> Module -> FValue -> [FValue] -> State.State IRCtx (Either TypeError Instruction)
make_call = error "not implemented yet"
-- branches {{{2
make_br_ret :: Br
make_br_ret = BrRet

make_br_goto :: BlockIdx -> Br
make_br_goto = BrGoto

make_br_cond :: Function -> Located FValue -> BlockIdx -> BlockIdx -> State.State IRCtx (Either TypeError Br)
make_br_cond fun (Located condsp cond) t f =
    to_state (type_of (fun, cond)) >>= \ cond_ty ->
    get_ds BoolType >>= \ bool_ty ->
    if cond_ty == bool_ty
          then return $ Right $ BrCond cond t f
          else return $ Left $ TypeError
                  ( ThingIs "branch condition's type" condsp cond_ty NoReason )
                  [ ThingShouldBe "branch condition's type" condsp bool_ty NoReason
                  ]
-- replace_block {{{1
replace_block :: [BasicBlock] -> Int -> BasicBlock -> [BasicBlock]
replace_block blocks idx block =
    let (keep, _:keep2) = splitAt idx blocks
    in keep ++ block : keep2
-- function cfg modification {{{1
add_basic_block :: String -> Function -> (BlockIdx, Function)
add_basic_block name fun =
    ( new_block_idx
    , fun { get_blocks = blocks ++ [new_block] }
    )
    where
        blocks = get_blocks fun
        new_block_idx = BlockIdx $ length blocks
        new_block = BasicBlock name [] Nothing
add_instruction :: Instruction -> BlockIdx -> Function -> (InstructionIdx, Function)
add_instruction instr (BlockIdx block_idx) fun = (instr_idx, fun { get_instruction_pool = new_pool, get_blocks = new_blocks })
    where
        new_pool = get_instruction_pool fun ++ [instr]
        instr_idx = InstructionIdx $ length new_pool - 1

        blocks = get_blocks fun

        (BasicBlock block_name block_instrs block_br) = blocks !! block_idx
        new_block = BasicBlock block_name (block_instrs ++ [instr_idx]) block_br
        new_blocks = replace_block blocks block_idx new_block

add_br :: Br -> BlockIdx -> Function -> Function
add_br br (BlockIdx block_idx) fun = fun { get_blocks = new_blocks }
    where
        blocks = get_blocks fun
        (BasicBlock block_name block_instrs _) = blocks !! block_idx
        new_block = BasicBlock block_name block_instrs (Just br)
        new_blocks = replace_block blocks block_idx new_block
-- cfg analysis {{{1
find_preds :: [BasicBlock] -> [(BlockIdx, [BlockIdx])]
find_preds blocks = preds
    where
        numbered_blocks :: [(BlockIdx, BasicBlock)]
        numbered_blocks = zip (map BlockIdx [0..]) blocks

        preds :: [(BlockIdx, [BlockIdx])]
        preds = map (\ (i, _) -> (i, preds_of i)) numbered_blocks

        preds_of :: BlockIdx -> [BlockIdx]
        preds_of b = map fst $ filter ((`is_pred_of` b) . snd) numbered_blocks

        is_pred_of :: BasicBlock -> BlockIdx -> Bool
        is_pred_of (BasicBlock _ _ (Just br)) dest = br `leads_to` dest
        is_pred_of (BasicBlock _ _ Nothing) _ = False

        leads_to :: Br -> BlockIdx -> Bool
        leads_to BrRet _ = False
        leads_to (BrGoto b) dest = b == dest
        leads_to (BrCond _ t f) dest = t == dest || f == dest
-- optimizations {{{1
-- helpers {{{2
keep_blocks :: [BasicBlock] -> [BlockIdx] -> [BasicBlock]
keep_blocks blocks indexes =
    let new_indexes = zip indexes (map BlockIdx [0..])

        convert_block_idx = fromJust . (`lookup` new_indexes)

        convert_br BrRet = BrRet
        convert_br (BrGoto b) = BrGoto (convert_block_idx b)
        convert_br (BrCond v t f) = BrCond v (convert_block_idx t) (convert_block_idx f)

        keep_block (BlockIdx block_idx) =
            let (BasicBlock name instrs br) = blocks !! block_idx
                new_br = convert_br <$> br
            in BasicBlock name instrs new_br

    in map keep_block indexes

repeat_opt :: (Function -> Function) -> Function -> Function
repeat_opt opt f =
    let f' = opt f
    in if f == f'
        then f
        else repeat_opt opt f'
-- simplify cfg {{{2
simplify_cfg :: Function -> Function
simplify_cfg = repeat_opt $ remove . merge
    where
        remove fun =
            let blocks = get_blocks fun
                preds = find_preds blocks
                keep = map fst $ filter (\ (idx, p) -> not (null p) || idx == get_entry_block fun || idx == get_exit_block fun) preds
            in fun { get_blocks = keep_blocks blocks keep }

        merge fun =
            let blocks = get_blocks fun
                preds = find_preds blocks
                mergeable =
                    mapMaybe (
                        \ (cur, ps) ->
                        if cur == get_entry_block fun || cur == get_exit_block fun
                            then Nothing
                            else case ps of
                                    [pidx@(BlockIdx pidx')] ->
                                        let (BasicBlock _ _ p_br) = blocks !! pidx'
                                        in case p_br of
                                            Just (BrGoto _) -> Just (cur, pidx)
                                            _ -> Nothing
                                    _ -> Nothing
                    ) preds

            in case mergeable of
                (BlockIdx block, BlockIdx merge_into):_ ->
                    let (BasicBlock name instrs1 _) = blocks !! merge_into
                        (BasicBlock _ instrs2 br) = blocks !! block
                        new_block = BasicBlock name (instrs1 ++ instrs2) br

                        new_blocks = replace_block blocks merge_into new_block
                    in fun { get_blocks = new_blocks }
                _ -> fun
-- printing functions {{{1
print_fun :: Function -> Reader.Reader IRCtx String
print_fun fun@(Function blocks _ _ regs (RegisterIdx ret_reg_idx) param_regs _ _ _ _) =
    concat <$> sequence (map stringify_reg $ zip ([0..] :: [Int]) regs) >>= \ strd_registers ->
    concat <$> sequence (map stringify_block $ zip (map BlockIdx [0..]) blocks) >>= \ strd_blocks ->

    return (concat
        [ "fun {\n"
        , strd_registers
        , "\n"
        , strd_blocks
        , "}\n"
        ])
    where
        make_comment tags = if null tags then "" else " // " ++ intercalate ", " tags

        stringify_reg (reg_idx, Register reg_ty _) =
            let tags = (
                        if ret_reg_idx == reg_idx
                            then ["return value register"]
                            else []
                    ) ++ (
                        case findIndex (\ (RegisterIdx i) -> i == reg_idx) param_regs of
                            Just i -> ["register for param " ++ show i]
                            Nothing -> []
                    )
            in resolve_dsidx reg_ty >>=
            stringify_ty >>= \ strd_reg_ty ->
            return $ "    " ++ "#" ++ show reg_idx ++ ": " ++ strd_reg_ty ++ ";" ++ make_comment tags ++ "\n"

        stringify_block (block_idx@(BlockIdx block_idx'), BasicBlock block_name instructions m_br) =
            concat <$> sequence (map (\ idx@(InstructionIdx idx') ->
                let instr = get_instruction fun idx
                in type_of instr >>= resolve_dsidx >>= stringify_ty >>= \ strd_instr_ty ->
                stringify_instr instr >>= \ strd_instr ->
                return ("        %" ++ show idx' ++ ": " ++ strd_instr_ty ++ " = " ++ strd_instr ++ ";\n")
            ) instructions) >>= \ strd_instrs ->

            maybe (return "<no br>") stringify_br m_br >>= \ strd_br ->

            return $
                "    "  ++ format_block_name_num block_name block_idx' ++ " {" ++ make_comment tags ++ "\n" ++
                strd_instrs ++
                "        =>: " ++ strd_br ++ ";\n" ++

                "    }\n"
            where
                preds = find_preds blocks
                format_block_name_num name num = name ++ "(" ++ show num ++ ")"
                stringify_block_ref (BlockIdx idx) =
                    let (BasicBlock name _ _) = blocks !! idx
                    in format_block_name_num name idx

                tags =
                    case lookup block_idx preds of
                        Just block_preds
                            | not $ null block_preds -> ["has predecessors: " ++ intercalate ", " (map stringify_block_ref block_preds)]
                        _ -> ["no predecessors"]

                stringify_reg_ref (RegisterIdx i) = '#' : show i

                stringify_fv (FVGlobalValue _) = error "not implemented yet" -- TODO: figure this out
                stringify_fv (FVNLVRegister i) = return $ stringify_reg_ref i
                stringify_fv (FVLValue lv) = return $ stringify_lv lv
                stringify_fv (FVConstBool b _) = return $ if b then "true" else "false"
                stringify_fv (FVConstChar c _) = return ['\'', c, '\'']
                stringify_fv (FVUnit _) = return "unit"
                stringify_fv (FVInstruction (InstructionIdx iidx)) = return $ "%" ++ show iidx

                stringify_fv (FVConstInt i ty) =
                    resolve_dsidx ty >>= stringify_ty >>= \ strd_ty ->
                    return ("(" ++ show i ++ " of " ++ strd_ty ++ ")")

                stringify_fv (FVConstFloat d ty) =
                    resolve_dsidx ty >>= stringify_ty >>= \ strd_ty ->
                    return ("(" ++ show d ++ " of " ++ strd_ty ++ ")")

                stringify_lv (LVRegister i) = stringify_reg_ref i

                stringify_instr (Copy lv fv _) =
                    stringify_fv fv >>= \ strd_fv ->
                    return ("copy " ++ strd_fv ++ " -> " ++ stringify_lv lv)
                stringify_instr (Call fv args) =
                    stringify_fv fv >>= \ strd_fv ->
                    sequence (map stringify_fv args) >>= \ strd_args ->
                    return ("call " ++ strd_fv ++ " [" ++ intercalate ", " strd_args ++ "]")

                stringify_br BrRet = return "ret"
                stringify_br (BrGoto b) = return $ "goto " ++ stringify_block_ref b
                stringify_br (BrCond fv t f) =
                    stringify_fv fv >>= \ strd_fv ->
                    return ("cond " ++ strd_fv ++ " " ++ stringify_block_ref t ++ " " ++ stringify_block_ref f)

            {-
                where
            -}
