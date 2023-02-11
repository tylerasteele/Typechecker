module Main where

import ParCPP ( pProgram, myLexer )
import System.Exit (die)
import AbsCPP
import qualified Data.Map as Map
import Data.Maybe


main :: IO ()
main = do
  c <- getContents
  case pProgram (myLexer c) of
    Left err -> putStrLn "parse error!" >> die err
    Right p -> case typecheck p of
      Left error -> putStrLn "Typecheck error encountered!" >> die error
      Right p -> print p

----------------------------
-- Environments and Types --
----------------------------

data CType = TFun (Type, [Type]) | TPrim Type deriving (Show, Eq)
data CEnv = CEnv [Map.Map Id CType] deriving (Show)


------------------
-- Type Checker --
------------------
typecheck :: Program -> Either String CEnv
typecheck p = do pInit p emptyEnv

pInit :: Program -> CEnv -> Either String CEnv
pInit (PDefs []) env = return env 
pInit (PDefs defs) env = do
  defRel <- dDefs defs env
  prgRel <- pDefs defs defRel  
  return prgRel

pDefs :: [Def] -> CEnv -> Either String CEnv 
pDefs [] env = return env 
pDefs (d:ds) env = do 
  env' <- pDef d env 
  pDefs ds env'
  

pDef :: Def -> CEnv -> Either String CEnv 
pDef (DFun t idF as ss) env = do 
  env'  <- dParams as (pushE env)
  env'' <- sSeq (TPrim t) ss env' 

  if any isReturn ss || t == Type_void  || idF == Id "main" then return env'' else Left "Missing Return!"

isReturn :: Stm -> Bool 
isReturn (SReturn _) = True 
isReturn (SReturnVoid) = True 
isReturn _ = False 

dDefs :: [Def] -> CEnv -> Either String CEnv
dDefs [] env = Right env
dDefs (d:ds) env = do
  env' <- dFun d env
  dDefs ds env'

dFun :: Def -> CEnv -> Either String CEnv
dFun (DFun t idF as _) e = do 
  env' <- dDecl (TFun (t, getAType as)) [idF] e
  Right env'            


dParams :: [Arg] -> CEnv -> Either String CEnv 
dParams []                 env = Right env 
dParams ((ADecl t idA):as) env = do 
  env' <- dDecl (TPrim t) [idA] env 
  dParams as env'

dDecl :: CType -> [Id] -> CEnv -> Either String CEnv
dDecl t [] env                 = Right env
--dDecl t (i:is) (CEnv [])       = let env' = CEnv [Map.insert i t mtMap] in dDecl t is env'
dDecl t (i:is) (CEnv (c:env)) 
  | Map.member i c             = Left "Redundant Declaration!"
  | otherwise                  = let env' = CEnv (Map.insert i t c:env) in dDecl t is env'

sBlock :: CType -> [Stm] -> CEnv -> Either String CEnv 
sBlock ty s c = do 
  sSeq ty s (pushE c)
  return c

sSeq :: CType ->  [Stm] -> CEnv -> Either String CEnv 
sSeq ty [] env = Right env 
sSeq ty (s:ss) env = do 
  env' <- typecheckStm ty s env 
  sSeq ty ss env'


typecheckStm :: CType -> Stm -> CEnv -> Either String CEnv 
typecheckStm ty stm env =  
  case stm of 
  SExp exp                             -> do 
                                            t <- typecheckExp exp env 
                                            Right env 

  SDecls ty ids                        -> do dDecl (TPrim ty) ids env 

  SInit ty id exp                      -> do
    env' <- dDecl (TPrim ty) [id] env 
    e    <- typecheckExp exp env' 
    if e == (TPrim ty) then return env' else do Left "Type Mismatch."

  SReturn exp                          -> do
    t1 <- typecheckExp exp env
    if t1 == ty then return env else Left "Return Types Are Misaligned!"

  SReturnVoid                          -> if ty == (TPrim Type_void ) then return env else Left "Return not valid!"-- head must be equal to void

  SWhile exp stms                     -> do 
    b <- typecheckExp exp env 
    if b == TPrim Type_bool then do 
      ss <- typecheckStm ty stms (pushE env)
      return env

    else 
      Left ("While Loop expression not boolean " ++ show b)

  SBlock stms                          -> sBlock ty stms env

  SIfElse exp stm1 stm2                -> do 
    b <- typecheckExp exp env 
    if b == TPrim Type_bool then do 
      s1 <- typecheckStm ty stm1 (pushE env)
      s2 <- typecheckStm ty stm2 (pushE env)
      return env
    else 
      Left ("if statement expression not boolean" ++ show b)


-- data CType = TFun (Type, [Type]) | TPrim Type deriving (Show, Eq)
typecheckExp :: Exp -> CEnv -> Either String CType  
typecheckExp exp env = case exp of 
  ETrue              -> Right (TPrim Type_bool)
  EFalse             -> Right (TPrim Type_bool)
  EInt n             -> Right (TPrim Type_int )
  EDouble x          -> Right (TPrim Type_double)
  EString s          -> Right (TPrim Type_string)
  
  EId id             -> do checkID id env  

  EApp id fs       -> do 
                          fType <- checkID id env
                          eTypes <- getExpTypes env fs 

                          case fType of 
                            (TPrim t) -> do Left "Expression Application is a Primitive"
                            (TFun (i, es)) -> do 
                              if es == eTypes then return (TPrim i) else do Left "Expression Application Parameters don't Align!"

  EPIncr exp1        -> do 
                          t <- typecheckExp exp1 env 
                          if t == TPrim Type_int || t == TPrim Type_double then return t else Left "Post increment type is not int or double!"

  EPDecr exp1        -> do 
                          t <- typecheckExp exp1 env 
                          if t == TPrim Type_int || t == TPrim Type_double then return t else Left "Post increment type is not int or double!"

  EIncr exp1         -> do 
                          t <- typecheckExp exp1 env 
                          if t == TPrim Type_int || t == TPrim Type_double then return t else Left "Post increment type is not int or double!"

  EDecr exp1         -> do 
                          t <- typecheckExp exp1 env 
                          if t == TPrim Type_int || t == TPrim Type_double then return t else Left "Post increment type is not int or double!"

  ETimes exp1 exp2   -> do 
                          t1 <- typecheckExp exp1 env 
                          t2 <- typecheckExp exp2 env 
                          let same = t1 == t2 
                          if same && (t1 == TPrim Type_int || t1 == TPrim Type_double) then return t1 else Left "Multiplication type error!"


  EDiv exp1 exp2     -> do 
                          t1 <- typecheckExp exp1 env 
                          t2 <- typecheckExp exp2 env 
                          let same = t1 == t2 
                          if same && (t1 == TPrim Type_int || t1 == TPrim Type_double) then return t1 else Left "Division type error!"


  EPlus exp1 exp2    -> do 
                          t1 <- typecheckExp exp1 env 
                          t2 <- typecheckExp exp2 env 
                          let same = t1 == t2 
                          if same && (t1 == TPrim Type_int || t1 == TPrim Type_double || t1 == TPrim Type_string ) then return t1 else Left "Division type error!"

  EMinus exp1 exp2   -> do 
                          t1 <- typecheckExp exp1 env 
                          t2 <- typecheckExp exp2 env 
                          let same = t1 == t2 
                          if same && (t1 == TPrim Type_int || t1 == TPrim Type_double) then return t1 else Left "Minus type error!"


  ELt exp1 exp2      -> do 
                          t1 <- typecheckExp exp1 env 
                          t2 <- typecheckExp exp2 env 
                          let same = t1 == t2 
                          if same && (t1 == TPrim Type_int || t1 == TPrim Type_double || t1 == TPrim Type_bool || t1 == TPrim Type_string ) then return (TPrim Type_bool ) else Left "Less than type error!"


  EGt exp1 exp2      -> do 
                          t1 <- typecheckExp exp1 env 
                          t2 <- typecheckExp exp2 env 
                          let same = t1 == t2 
                          if same && (t1 == TPrim Type_int || t1 == TPrim Type_double || t1 == TPrim Type_bool || t1 == TPrim Type_string ) then return (TPrim Type_bool ) else Left "Greater than type error!"


  ELtEq exp1 exp2    -> do 
                          t1 <- typecheckExp exp1 env 
                          t2 <- typecheckExp exp2 env 
                          let same = t1 == t2 
                          if same && (t1 == TPrim Type_int || t1 == TPrim Type_double || t1 == TPrim Type_bool || t1 == TPrim Type_string ) then return (TPrim Type_bool ) else Left "<= type error!"


  EGtEq exp1 exp2    -> do 
                          t1 <- typecheckExp exp1 env 
                          t2 <- typecheckExp exp2 env 
                          let same = t1 == t2 
                          if same && (t1 == TPrim Type_int || t1 == TPrim Type_double || t1 == TPrim Type_bool || t1 == TPrim Type_string ) then return (TPrim Type_bool ) else Left ">= type error!"


  EEq exp1 exp2      -> do 
                          t1 <- typecheckExp exp1 env 
                          t2 <- typecheckExp exp2 env 
                          let same = t1 == t2 
                          if same && (t1 == TPrim Type_int || t1 == TPrim Type_double || t1 == TPrim Type_bool || t1 == TPrim Type_string ) then return (TPrim Type_bool ) else Left "Equality type error! (==)"


  ENEq exp1 exp2     -> do 
                          t1 <- typecheckExp exp1 env 
                          t2 <- typecheckExp exp2 env 
                          let same = t1 == t2 
                          if same && (t1 == TPrim Type_int || t1 == TPrim Type_double || t1 == TPrim Type_bool || t1 == TPrim Type_string ) then return (TPrim Type_bool ) else Left "Not equals type error! (!=)"


  EAnd exp1 exp2     ->  do 
                          t1 <- typecheckExp exp1 env 
                          t2 <- typecheckExp exp2 env 
                          let same = t1 == t2 
                          if same && (t1 == TPrim Type_bool) then return t1 else Left "\"And\" type error! (&&)"


  EOr exp1 exp2      -> do 
                          t1 <- typecheckExp exp1 env 
                          t2 <- typecheckExp exp2 env 
                          let same = t1 == t2 
                          if same && (t1 == TPrim Type_bool) then return t1 else Left "\"Or\" type error! (||)"


  EAss exp1 exp2     -> do 
                          t1 <- typecheckExp exp1 env 
                          t2 <- typecheckExp exp2 env 
                          let same = t1 == t2 
                          if same then return t1 else Left ("Assignment type error! " ++ show t1 ++ " not equal to " ++ show t2)



-----------------
-- Helper Fun! --
-----------------
getAType :: [Arg] -> [Type]
getAType [] = []
getAType ((ADecl t id) : as) = t : getAType as

mtMap :: Map.Map Id CType 
mtMap = Map.empty  

pushE :: CEnv -> CEnv 
pushE (CEnv env) = CEnv (Map.empty : env)

popE :: CEnv -> CEnv 
popE (CEnv (c:cs)) = CEnv cs

returnsCheck :: [Stm] -> Type -> Either String Bool  
returnsCheck s Type_void = Right True
returnsCheck s t = retCheckT s t

retCheckT :: [Stm] -> Type -> Either String Bool
retCheckT [] t = Left "Return Statement Not Found!" 
retCheckT (s:ss) t = do 
  if s == SReturnVoid then --------- || s == SReturn (( Syntax for SReturn *  ))
    Right True 
  else 
    retCheckT ss t 

checkID :: Id ->  CEnv -> Either String CType 
checkID (Id s) (CEnv []) = Left (s ++ " not declared!")
checkID id (CEnv (ctx:env)) 
  | Map.member id ctx = Right $ fromJust (Map.lookup id ctx) 
  | otherwise = checkID id (CEnv env)

getExpTypes :: CEnv -> [Exp] -> Either String [Type]
getExpTypes env [] = return []
getExpTypes env (e:es) = do 
  t1 <- typecheckExp e env 
  ts <- getExpTypes env es 

  case t1 of 
    (TFun f) -> Left "Expression Application Error!" 
    (TPrim typ) -> do 
      return (typ:ts)

-------------
-- Testing --
------------- 
emptyEnv :: CEnv
emptyEnv = CEnv [Map.empty] 

testP :: Program
testP = PDefs [testD, testD]

testD :: Def
testD = DFun Type_int  (Id "main") [] []

syntax :: Def
syntax = DFun Type_void  (Id "xy") [ADecl Type_int (Id "var")] [SExp ETrue]