module CBackend
    ( lower_mod_to_c
    ) where

import qualified IR
import qualified Mangle

import StateAndLens

import Interner

import Data.List (intercalate)

import qualified Control.Monad.Reader as Reader (Reader)
import Control.Monad (filterM, liftM)

lower_mod_to_c :: IR.Module -> Reader.Reader IR.IRCtx String
lower_mod_to_c _ =
    interner_idxs <$> view_r IR.ds_interner >>= \ dses ->
    interner_idxs <$> view_r IR.v_interner >>= \ vals ->
    let include_section =
            "// includes:\n" ++
            concatMap (("#include "++) . (++"\n"))
                [ "<stdint.h>"
                ] ++
            "\n"
    in
    lower_list "declaration" Mangle.mangle_ds_interner_idx lower_ds dses >>= \ (ddecl_ds, ddef_ds) ->
    lower_list "value" Mangle.mangle_v_interner_idx lower_v vals >>= \ (vdecl_v, vdef_v) ->

    dlower_irctx >>= \ (ddecl_irctx, ddef_irctx) ->
    vlower_irctx >>= \ (vdecl_irctx, vdef_irctx) ->

    let ddecl_irctx' = "// declaration declaration of irctx:\n" ++ ddecl_irctx ++ "\n"
        ddef_irctx' = "// declaration definition of irctx:\n" ++ ddef_irctx ++ "\n"
        vdecl_irctx' = "// value declaration of irctx:\n" ++ vdecl_irctx ++ "\n"
        vdef_irctx' = "// value definition of irctx:\n" ++ vdef_irctx ++ "\n"
    in return (concat
        [ include_section

        , ddecl_ds
        , ddecl_irctx'

        , ddef_ds
        , ddef_irctx'

        , vdecl_v
        , vdecl_irctx'

        , vdef_v
        , vdef_irctx'
        ])

type LoweringFun entity = Mangle.MangledName -> entity -> Reader.Reader IR.IRCtx (String, String)

not_necessary :: String
not_necessary = "// not necessary\n"
not_necessary' :: (String, String)
not_necessary' = (not_necessary, not_necessary)

-- lowering lists and adding comments {{{1
lower_list :: Show a => String -> (a -> Mangle.MangledName) -> LoweringFun a -> [a] -> Reader.Reader IR.IRCtx (String, String)
lower_list thing_type mangler lowerer items =
    unzip <$> sequence (map lower_thing items) >>= \ (decls, defs) ->
    return
        ( group_comment "declaration" ++ concat decls
        , group_comment "definition" ++ concat defs
        )
    where
        lower_thing thing =
            let mangled_name = mangler thing
            in lowerer mangled_name thing >>= \ (decl, def) ->
            return
                ( single_comment "declaration" thing mangled_name ++ decl ++ "\n"
                , single_comment "definition" thing mangled_name ++ def ++ "\n"
                )

        single_comment action thing mangled_name = "// " ++ action ++ " of " ++ thing_type ++ " '" ++ show thing ++ "' mangled as '" ++ Mangle.mangled_str mangled_name ++ "':\n"
        group_comment action = "// " ++ action ++ "s of " ++ thing_type ++ "s\n\n"

-- helpers {{{1
filter_units_from_type_list :: [IR.DSIdx IR.Type] -> Reader.Reader IR.IRCtx [IR.DSIdx IR.Type]
filter_units_from_type_list typelist =
    let is_unit ty =
            IR.resolve_dsidx ty >>= \case
                IR.UnitType -> return True
                _ -> return False
    in filterM (liftM not . is_unit) typelist
-- c declarations {{{1
data CDecl = CDecl String CDeclarator
data CDeclarator
    = IdentifierDeclarator Mangle.MangledName
    | AbstractDeclarator
    | PointerDeclarator CDeclarator
    | ArrayDeclarator CDeclarator Int
    | FunctionDeclarator CDeclarator [CDecl]

str_cdecl :: CDecl -> String
str_cdecl (CDecl res declarator) =
    case str_declarator declarator of
        "" -> res
        s -> res ++ " " ++ s
    where
        str_declarator' d = "(" ++ str_declarator d ++ ")"

        str_declarator (IdentifierDeclarator mn) = Mangle.mangled_str mn
        str_declarator AbstractDeclarator = ""
        str_declarator (PointerDeclarator decl) = "*" ++ str_declarator' decl
        str_declarator (ArrayDeclarator decl size) = str_declarator' decl ++ "[" ++ show size ++ "]"
        str_declarator (FunctionDeclarator decl []) = str_declarator' decl ++ "(void)"
        str_declarator (FunctionDeclarator decl params) = str_declarator' decl ++ "(" ++ intercalate ", " (map str_cdecl params) ++ ")"
-- converting types to c declarations {{{1
type_to_cdecl' :: IR.DSIdx IR.Type -> Maybe Mangle.MangledName -> Reader.Reader IR.IRCtx CDecl
type_to_cdecl' idx name =
    IR.resolve_dsidx idx >>= \ ty ->
    type_to_cdecl ty name

type_to_cdecl :: IR.Type -> Maybe Mangle.MangledName -> Reader.Reader IR.IRCtx CDecl
type_to_cdecl ty name =
    let name_declarator = maybe AbstractDeclarator IdentifierDeclarator name
    in add_to_declarator False name_declarator ty >>= \ (basic_type, declarator) ->
    return (CDecl basic_type declarator)

-- uav = unit_as_void
add_to_declarator' :: Bool -> CDeclarator -> IR.DSIdx IR.Type -> Reader.Reader IR.IRCtx (String, CDeclarator)
add_to_declarator' uav parent idx = IR.resolve_dsidx idx >>= add_to_declarator uav parent

add_to_declarator :: Bool -> CDeclarator -> IR.Type -> Reader.Reader IR.IRCtx (String, CDeclarator)
-- the given declarator is a value that is the same type as the type passed in
-- this function will add to the declarator the operation that can be performed on the type, and also return the basic type
add_to_declarator _ parent (IR.FloatType IR.F32) = return ("float", parent)
add_to_declarator _ parent (IR.FloatType IR.F64) = return ("double", parent)
add_to_declarator _ parent (IR.IntType size signedness) =
    let signedness_str = case signedness of
            IR.Signed -> ""
            IR.Unsigned -> "u"
    in return (signedness_str ++ "int" ++ show size ++ "_t", parent)

add_to_declarator unit_as_void parent IR.UnitType
    | unit_as_void = return ("void", parent)
    | otherwise = error "cannot lower unit type to c"

add_to_declarator _ parent IR.CharType = return ("uint8_t", parent) -- TODO: this maybe should not be 8 bits
add_to_declarator _ parent IR.BoolType = return ("int", parent)

add_to_declarator _ parent (IR.FunctionPointerType ret params) =
    filter_units_from_type_list params >>= \ params' ->
    sequence (map (\ pty -> type_to_cdecl' pty Nothing) params') >>= \ params'' ->
    let parent' = FunctionDeclarator (PointerDeclarator parent) params''
    in add_to_declarator' True parent' ret

-- convert parent (IR.PointerType _ pointee) =
    -- let parent' = PointerDeclarator parent
    -- in convert' parent' pointee
-- declsymbols {{{1
lower_ds :: LoweringFun (InternerIdx IR.DeclSymbol')
lower_ds mname idx =
    view_r IR.ds_interner >>=
    IR.apply_to_ds (error "cannot lower module in c backend") (lower_ty mname) . resolve_interner_idx idx

lower_ty :: LoweringFun IR.Type
lower_ty _ IR.UnitType = return not_necessary'
lower_ty mname ty =
    type_to_cdecl ty (Just mname) >>= \ cdecl ->
    return ( "typedef " ++ str_cdecl cdecl ++ ";\n"
    , case ty of
        IR.FloatType _ -> not_necessary
        IR.IntType _ _ -> not_necessary
        IR.CharType -> not_necessary
        IR.BoolType -> not_necessary
        IR.FunctionPointerType _ _ -> not_necessary
        IR.UnitType -> not_necessary
    )
-- declsymbol phase of irctx {{{1
dlower_irctx :: Reader.Reader IR.IRCtx (String, String)
dlower_irctx = return not_necessary'
-- values {{{1
lower_v :: LoweringFun (InternerIdx IR.Value')
lower_v mname idx =
    view_r IR.v_interner >>=
    IR.apply_to_v (lower_fun_ptr mname) . resolve_interner_idx idx

lower_fun_ptr :: LoweringFun IR.ConstFunctionPointer
lower_fun_ptr mname fptr =
    IR.type_of fptr >>= \ ty ->
    type_to_cdecl' ty (Just mname) >>= \ cdecl ->
    let cdecl_strd = str_cdecl cdecl
    in return
        ( cdecl_strd ++ ";\n"
        , cdecl_strd ++ " = &" ++ Mangle.mangled_str (Mangle.mangle_fun (IR.get_function_idx fptr)) ++ ";\n"
        )
-- value phase of irctx {{{1
vlower_irctx :: Reader.Reader IR.IRCtx (String, String)
vlower_irctx =
    view_r IR.function_interner >>= \ function_interner ->
    let function_idxs = interner_idxs function_interner
    in lower_list "function" Mangle.mangle_fun lower_fun function_idxs
-- functions {{{1
lower_fun :: LoweringFun (InternerIdx IR.Function)
lower_fun mname funidx =
    view_r IR.function_interner >>= \ function_interner ->
    let fun = resolve_interner_idx funidx function_interner

        ret_ty = IR.get_ret_type fun
    in filter_units_from_type_list (IR.get_param_types fun) >>= \ param_tys ->

    sequence (map (\ tyidx -> type_to_cdecl' tyidx Nothing) param_tys) >>= \ param_tys_as_decls ->
    add_to_declarator' True (FunctionDeclarator (IdentifierDeclarator mname) param_tys_as_decls) ret_ty >>= \ (fun_basic_type, fun_declarator) ->

    let cdecl_strd = str_cdecl (CDecl fun_basic_type fun_declarator)

    in return
        ( cdecl_strd ++ ";\n"
        , cdecl_strd ++ " {\n    #error not implemented yet\n}\n"
        )
