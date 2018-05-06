{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}

module While where

import            Prelude                   hiding (lookup)
import            Control.Applicative       (Applicative, Alternative,
                                             empty, (<|>), some, many, optional)
import            Control.Monad             (forM_, (<=<), when, liftM2, forever, foldM, join)
import            Control.Monad.IO.Class    (MonadIO, liftIO)
import            Control.Monad.State.Lazy  (StateT, MonadIO, liftIO, get, put, evalStateT)
import            System.IO                 (hPutStrLn, hSetBuffering, stdout, stderr, BufferMode(..), isEOF)
import            System.IO.Error           (tryIOError)
import            System.Environment        (getArgs)
import            System.Exit               (exitSuccess)
import            Data.Bifunctor            (first, second)
import            Data.Maybe                (fromMaybe)
import            Data.Char                 (isAlpha, isAlphaNum)
import            Data.List                 (intercalate)
import            Data.Map.Lazy             (Map, member, insert, alter, (!?))
import qualified  Data.Map.Lazy             as Map
import            Parser
import            Parser.Result             (SingleResult(..))

data LVal
  = Arr LVal Aexp
  | Var Var

-- | This is syntactic equality, not value or even semantic equality.
-- This means that `x[(1 + 2) + 3]` and `x[1 + (2 + 3)]` will not compare as
-- equal.
deriving instance Eq LVal

data Aexp
  = Num Int
  | LVal LVal
  | Aexp :+: Aexp
  | Aexp :*: Aexp
  | Aexp :-: Aexp
-- | This is  to syntactic equality, not value or even semantic equality.
-- This means that `(1 + 2) + 3` and `1 + (2 + 3)` will not compare as equal.
deriving instance Eq Aexp

data Bexp
  = T
  | F
  | Aexp :=: Aexp
  | Aexp :<: Aexp
  | Aexp :>: Aexp
  | Bexp :&: Bexp
  | Not Bexp

type Var = String
type Args = ([Var], [(Var, Aexp)], Maybe Var)
type VarParam = Maybe (LVal, Aexp)
type Params = ([Aexp], VarParam)

data Function = Args :-> Stm

data FrameType = BlockFrame | Loop | Function

type Frame = (FrameType, Map Var Int, Map Var Function)

type Stack = [Frame]
emptyStack :: Stack
emptyStack = [(BlockFrame, Map.empty, Map.empty)]

data Stm
  = LVal := Aexp
  | Decl Var Aexp
  | Proc Var Args Stm
  | If Bexp Stm Stm
  | While Bexp Stm
  | Block [Stm]
  | Call Var Params
  --- | Next
  --- | Break
  --- | Return

indent :: Int -> String
indent i = replicate (2*i) ' '

instance Show LVal where
  show (Var v) = v
  show (Arr v i) = show v ++ "[" ++ show i ++ "]"

instance Show Aexp where
  show (Num n) = show n
  show (LVal v) = show v
  show (x :+: y) = show x ++ " + " ++ show y
  show (x :*: y) = show x ++ " * " ++ show y
  show (x :-: y) = show x ++ " - " ++ show y

instance Show Bexp where
  show T = "true"
  show F = "false"
  show (x :=: y) = show x ++ " == " ++ show y
  show (x :<: y) = show x ++ " < " ++ show y
  show (x :>: y) = show x ++ " > " ++ show y
  show (x :&: y) = show x ++ " & " ++ show y
  show (Not (x :=: y)) = show x ++ " != " ++ show y
  show (Not (x :<: y)) = show x ++ " >= " ++ show y
  show (Not (x :>: y)) = show x ++ " <= " ++ show y
  show (Not (x :&: y)) = show (Not x) ++ " | " ++ show (Not y)
  show (Not (Not x)) = show x
  show (Not T) = show F
  show (Not F) = show T

showArgs :: Args -> String
showArgs (r, o, v) =
  "(" ++ intercalate ", " (r ++ fmap showOptArg o ++ maybe [] ((:[]) . (++"@")) v) ++ ")"
  where
    showOptArg (v,x) = v ++ ": " ++ show x

showParams :: Params -> String
showParams (p, vp) =
  "(" ++ intercalate ", " (fmap show p ++ showVarParam vp) ++ ")"
  where
    showVarParam Nothing = []
    showVarParam (Just (xs,n))
      | LVal xs == n = [show xs ++ "@"]
      | LVal xs == n = [show xs ++ "@" ++ show n]

showStm :: Int -> Stm -> String
showStm i (v := (x :+: y)) | LVal v == x = show v ++ " += " ++ show y ++ ";"
showStm i (v := (x :-: y)) | LVal v == x = show v ++ " -= " ++ show y ++ ";"
showStm i (v := (x :*: y)) | LVal v == x = show v ++ " *= " ++ show y ++ ";"
showStm i (v := x) = show v ++ " = " ++ show x ++ ";"
showStm i (Decl v x) = "var " ++ v ++ " = " ++ show x ++ ";"
showStm i (Proc v args s1) = "proc " ++ show v ++ showArgs args ++ " " ++ showStm i s1
showStm i (If b s1 (Block [])) = "if " ++ show b ++ " do " ++ showStm i s1
showStm i (If b s1@(Block _) s2) = "if " ++ show b ++ " then " ++ showStm i s1 ++ " else " ++ showStm i s2
showStm i (If b s1 s2) = "if " ++ show b ++ " then " ++ showStm i s1 ++ "\n" ++ indent i ++ "else " ++ showStm i s2
showStm i (While b s1) = "while " ++ show b ++ " do " ++ showStm i s1
showStm i (Block []) = "{}"
showStm i (Block xs) = "{" ++ concatMap ((("\n" ++ indent (i+1)) ++) . showStm (i+1)) xs ++ "\n" ++ indent i ++ "}"
showStm i (Call v params) = v ++ showParams params ++ ";"
-- showStm i Next = "next;"
-- showStm i Break = "break;"
-- showStm i Return = "return;"

-- data Flow = NextLoop | BreakLoop | Proceed | ReturnFunction

instance Show Stm where show = showStm 0

{- Variables are global unless defined at the beginning of a block.
   Procs are always local, and their scope is equal to their location.
   As they cannot be returned, there is no concept of closure.
-}

precedence :: [Parser (a -> a -> a)] -> Parser a -> Parser a
precedence ops arg = foldl build arg ops
  where build term ops = chainl1 term ops

aops :: [Parser (Aexp -> Aexp -> Aexp)]
aops = [(:*:) <$ tok "*"
       ,(:+:) <$ tok "+" <|> (:-:) <$ tok "-" ]

aexp = precedence aops
     $ Num <$> num
   <|> LVal <$> lval
   <|> tok "(" *> aexp <* tok ")"

bexp :: Parser Bexp
bexp = precedence [(||:) <$ tok "|"
                  ,(:&:) <$ tok "&"]
     $ simpleBexp

simpleBexp :: Parser Bexp
simpleBexp = T <$ word "true"
         <|> F <$ word "false"
         <|> (:=:) <$> aexp <* tok "==" <*> aexp
         <|>  neq  <$> aexp <* tok "!=" <*> aexp
         <|>  lte  <$> aexp <* tok "<=" <*> aexp
         <|> (:<:) <$> aexp <* tok "<" <*> aexp
         <|>  gte  <$> aexp <* tok ">=" <*> aexp
         <|> (:>:) <$> aexp <* tok ">" <*> aexp
         <|> Not <$ tok "!" <*> simpleBexp
         <|> tok "(" *> bexp <* tok ")"

neq :: Aexp -> Aexp -> Bexp
x `neq` y = Not (x :=: y)

lte :: Aexp -> Aexp -> Bexp
x `lte` y = Not (x :>: y)

gte :: Aexp -> Aexp -> Bexp
x `gte` y = Not (x :<: y)

(||:) :: Bexp -> Bexp -> Bexp
x ||: y = Not (Not x :&: Not y)

arrayRoot :: Var -> Var
arrayRoot = takeWhile (/= ';')

skip :: Stm
skip = Block []

stms :: Parser [Stm]
stms = many semidStm

unsemidStm :: Parser Stm
unsemidStm = simpleStm <|> compoundStm unsemidStm

unsemidNonBlock :: Parser Stm
unsemidNonBlock = simpleStm <|> nonBlock unsemidStm

semidStm :: Parser Stm
semidStm = simpleStm <* tok ";" <|> compoundStm semidStm <|> skip <$ tok ";"

semidNonBlock :: Parser Stm
semidNonBlock = simpleStm <* tok ";" <|> nonBlock semidStm <|> skip <$ tok ";"

loopCond :: Parser Bexp
loopCond = word "while" *> bexp
       <|> Not <$ word "until" <*> bexp

commaOrEnd :: String -> Parser ()
commaOrEnd end = () <$ (tok "," <|> ahead (tok end))

varParam :: Parser VarParam
varParam = optional $ f <$> lval <* tok "@" <*> optional aexp
  where
    f v Nothing  = (v, LVal v)
    f v (Just n) = (v,      n)

args :: Parser Args
args = (,,) <$ tok "(" <*> reqArgs <*> optArgs <*> varArg <* tok ")"
  where
    reqArgs = many (var <* commaOrEnd ")")
    optArgs = many ((,) <$> var <* tok ":" <*> aexp <* (commaOrEnd ")"))
    varArg = Just <$> var <* tok "@" <|> pure Nothing

params :: Parser Params
params = (,) <$ tok "(" <*> many (aexp <* commaOrEnd ")") <*> varParam <* tok ")"

compoundStm :: Parser Stm -> Parser Stm
compoundStm subStm = Block <$ tok "{" <*> stms <* tok "}"
                 <|> nonBlock subStm

nonBlock :: Parser Stm -> Parser Stm
nonBlock subStm
    = If <$ word "if" <*> bexp
         <* word "then" <*> unsemidStm <* optional (tok ";")
         <* word "else" <*> subStm
  <|> If <$ word "if" <*> bexp
         <* word "do" <*> subStm
         <*> pure skip
  <|> While <$> loopCond <* word "do" <*> subStm
  <|> Proc  <$ word "proc" <*> var <*> args <*> subStm

simpleStm :: Parser Stm
simpleStm 
    = (:=) <$> lval <* tok "=" <*> aexp
  <|> Decl <$ word "var" <*> var <* tok "=" <*> aexp
  <|> repeatWhile <$ word "repeat"
                  <*> ((:[]) <$> unsemidNonBlock <* optional (tok ";") <|> tok "{" *> stms <* tok "}")
                  <*> loopCond
  <|> modAssn <$> lval <*> foldr (<|>) empty aops <* tok "=" <*> aexp
  -- <|> Next <$ word "next"
  -- <|> Break <$ word "break"
  -- <|> Return <$ word "return"
  <|> Call <$> var <*> params

modAssn :: LVal -> (Aexp -> Aexp -> Aexp) -> Aexp -> Stm
modAssn var op val = var := (LVal var `op` val)

repeatWhile :: [Stm] -> Bexp -> Stm
-- repeatWhile s cond = While T . Block $ s ++ [If (Not cond) Break skip]
repeatWhile s cond = Block $ s ++ [While cond (Block s)]

num :: Parser Int
num = (read .) . (++)  <$> (tok "-" <|> pure "") <*> some (oneOf ['0' .. '9']) <* whitespace

keywords = ["while", "if", "then", "proc", "skip", "true", "false", "else", "do"]

wordChar c = c `elem` "$_" || isAlphaNum c
firstChar c = c `elem` "$_" || isAlpha c

endWord :: Parser ()
endWord = end <|> () <$ ahead (satisfy (not . wordChar))

var :: Parser Var
var = let px = satisfy firstChar <:> many (satisfy wordChar) <* endWord  <* whitespace
       in px >>= \name -> if name `elem` keywords then empty else return name

word :: String -> Parser Var
word str = string str <* endWord <* whitespace

lval :: Parser LVal
lval = chainl0 (Var <$> var) (tok "[" *> aexp <* tok "]") (pure Arr)
  where
    chainl0 :: Parser a -> Parser b -> Parser (a->b->a) -> Parser a
    chainl0 p0 p op = p0 >>= rest where
      rest x = do f <- op
                  y <- p
                  rest (f x y)
           <|> return x

whitespace :: Parser ()
whitespace = () <$ many (oneOf " \t\n\r") <* optional comment

comment :: Parser ()
comment = () <$ char '#' <* many (noneOf "\r\n") <* (() <$ end <|> () <$ oneOf "\r\n") <* whitespace

tok :: String -> Parser String
tok t = string t <* whitespace

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e = maybe (Left e) Right

lookupVar :: Map Var Int -> Var -> Maybe Int
lookupVar mvi var
  | member (arrayRoot var) mvi = mvi !? var
  | otherwise                  = Nothing

lookup :: Stack -> Var -> Maybe Int
lookup [] _ = Nothing
lookup ((ft, mvi, _):ss) var = lookupVar mvi var <|> lookup ss var

pop :: Stack -> Stack
pop = tail

var_name :: LVal -> Stack -> Either String Var
var_name (Var v) s = Right v
var_name (Arr v i) s = do
  name <- var_name v s
  index <- a_val i s
  return $ name ++ ";" ++ show index

a_val :: Aexp -> Stack -> Either String Int
a_val (Num n) s = Right n
a_val (a1 :+: a2) s = liftM2 (+) (a_val a1 s) (a_val a2 s)
a_val (a1 :-: a2) s = liftM2 (-) (a_val a1 s) (a_val a2 s)
a_val (a1 :*: a2) s = liftM2 (*) (a_val a1 s) (a_val a2 s)
a_val (LVal v) s = do
  name <- var_name v s
  maybeToEither ("Variable not found: " ++ showVar v) $ lookup s name
  where
    showVar (Var v) = v
    showVar (Arr v i) = showVar v ++ "[" ++ either (const "") show (a_val i s) ++ "]"

b_val :: Bexp -> Stack -> Either String Bool
b_val T s = Right True
b_val F s = Right False
b_val (Not b) s = not <$> b_val b s
b_val (a1 :=: a2) s = liftM2 (==) (a_val a1 s) (a_val a2 s)
b_val (a1 :<: a2) s = liftM2 (<)  (a_val a1 s) (a_val a2 s)
b_val (a1 :>: a2) s = liftM2 (>)  (a_val a1 s) (a_val a2 s)
b_val (b1 :&: b2) s = liftM2 (&&) (b_val b1 s) (b_val b2 s)

declare :: Stack -> Var -> Int -> Stack
declare ((ft, vs, ps):ss) v x = (ft, insert v x vs, ps) : ss

update :: Stack -> Var -> Int -> Stack
update [(ft, vs, ps)] v x = [(ft, insert v x (alter (<|> Just 1) (arrayRoot v) vs), ps)]
update ((ft, vs, ps):ss) v x
  | member (arrayRoot v) vs = (ft, insert v x vs, ps) : ss
  | otherwise               = (ft, vs, ps) : update ss v x

declareProc :: Stack -> Var -> Args -> Stm -> Stack
declareProc ((ft, vs, ps):ss) v args s1 = (ft, vs, insert v (args :-> s1) ps) : ss

mapMFst :: Monad m => (a -> m c) -> (a, b) -> m (c, b)
mapMFst = (fstSeq .) . first

bnidFst :: Monad m => (a -> m c) -> m (a, b) -> m (c, b)
bnidFst = ((=<<) . mapMFst)

bindFst :: Monad m => m (a, b) -> (a -> m c) -> m (c, b)
bindFst = flip bnidFst

fstSeq :: Monad m => (m a, b) -> m (a, b)
fstSeq (x,y) = (,y) <$> x

mapMSnd :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
mapMSnd = (sndSeq .) . second

bnidSnd :: Monad m => (b -> m c) -> m (a, b) -> m (a, c)
bnidSnd = ((=<<) . mapMSnd)

bindSnd :: Monad m => m (a, b) -> (b -> m c) -> m (a, c)
bindSnd = flip bnidSnd

sndSeq :: Monad m => (a, m b) -> m (a, b)
sndSeq (x,y) = (x,) <$> y

bindArgs :: Var -> Args -> Params -> Stack -> Either String (Stack, [Stm])
bindArgs fn args params s = first ((: s) . (Function, , Map.empty)) <$> (bindReqArgs 1 args =<< params')
  where
    params' :: Either String ([Int], Maybe (Var, Int))
    params' = bnidSnd getVarParam $ mapMFst (mapM (flip a_val s)) params -- TODO: Can this be changed to mapMSnd?

    getVarParam :: VarParam -> Either String (Maybe (Var, Int))
    getVarParam Nothing = Right Nothing
    getVarParam (Just (xs, n)) = (Just .) . (,) <$> var_name xs s <*> a_val n s


    bindReqArgs :: Int -> Args -> ([Int], Maybe (Var, Int)) -> Either String (Map Var Int, [Stm])
    bindReqArgs i ([],os,v) p = bindOptArgs i (os,v) p
    bindReqArgs i (r:rs,os,v) ([],  vp) = Left $ "Missing argument #" ++ show i ++ " to function " ++ fn ++ ": " ++ r
    bindReqArgs i (r:rs,os,v) (p:ps,vp) = do
      (vars, s1) <- bindReqArgs (succ i) (rs,os,v) (ps,vp)
      return (insert r p vars, s1)
    
    bindOptArgs :: Int -> ([(Var, Aexp)], Maybe Var) -> ([Int], Maybe (Var, Int)) -> Either String (Map Var Int, [Stm])
    bindOptArgs i ([],v) (ps,vp) = bindVarArg (succ i) v (ps, vp)
    bindOptArgs i ((o,x):os,v) ([],vp) = second ((Var o := x) :) <$> bindOptArgs (succ i) (os,v) ([],vp)
    bindOptArgs i ((o,x):os,v) (p:ps,vp) = first (insert o p) <$> bindOptArgs (succ i) (os,v) (ps, vp)

    bindVarArg :: Int -> Maybe Var -> ([Int], Maybe (Var, Int)) -> Either String (Map Var Int, [Stm])
    bindVarArg i Nothing ([],Nothing) = bindmentBase
    bindVarArg i Nothing (p:ps,vp) = Left $ "Unexpected argument #" ++ show i ++ " to function " ++ fn
    bindVarArg i Nothing ([],Just _) = Left $ "Unexpected vararg to function " ++ fn
    bindVarArg i _ (p:ps,Just _) = Left $ "Unexpected argument #" ++ show i ++ " to function " ++ fn
    bindVarArg i (Just v) ([],Just (vp, n)) = first (insert v n) <$> bindVarArgArray (n-1) v vp
    bindVarArg i (Just v) (vp, Nothing) = first (insert v (length vp) . insert (v ++ ";") 0) <$> bindVarArgInline 0 v vp

    bindVarArgArray :: Int -> Var -> Var -> Either String (Map Var Int, [Stm])
    bindVarArgArray i v vp
      | i < 0     = bindmentBase
      | otherwise = bindVarArgArray (pred i) v vp `bindFst` \vars -> do
          x <- a_val (LVal (Arr (Var vp) (Num i))) s
          return (insert (v++";"++show i) x vars)
    
    bindVarArgInline :: Int -> Var -> [Int] -> Either String (Map Var Int, [Stm])
    bindVarArgInline i v [] = bindmentBase
    bindVarArgInline i v (p:ps) = first (insert (v++";"++show i) p) <$> bindVarArgInline (succ i) v ps

    bindmentBase :: Either String (Map Var Int, [Stm])
    bindmentBase = Right (Map.empty, [])

prependBindings :: [Stm] -> Stm -> Stm
prependBindings bindings (Block ss) = Block (bindings ++ ss)
prependBindings bindings s1 = Block (bindings ++ [s1])

callProc :: Stack -> Var -> Params -> (Stm -> Stack -> Either String Stack) -> Either String Stack
callProc [] p _ _ = Left ("Function not found: "++p)
callProc s@((ft, vs, ps):ss) p xs k = case ps !? p of
  Just (args :-> s1) -> pop <$> (bindArgs p args xs s >>= \(stack, bindings) -> k (prependBindings bindings s1) stack)
  Nothing            -> ((ft, vs, ps):) <$> callProc ss p xs k

cond :: Monad m => (a->m Bool) -> (a->m b) -> (a->m b) -> (a->m b)
cond p t f x = p x >>= \b -> if b then t x else f x

fix :: (a -> a) -> a
fix f = let x = f x in x

s_ds :: Stm -> Stack -> Either String Stack
s_ds (     v :=    x) s = update s <$> var_name v s <*> a_val x s
s_ds (Decl v       x) s = declare s v <$> a_val x s
s_ds (If     p s1 s2) s = cond (b_val p) (s_ds s1) (s_ds s2) s
s_ds (While  p    s1) s = fix (\f -> cond (b_val p) (f <=< s_ds s1) return) s
s_ds (Block       []) s = Right s
s_ds (Block       ss) s = pop <$> foldM (flip s_ds) ((BlockFrame, Map.empty, Map.empty) : s) ss
s_ds (Proc v args s1) s = pure $ declareProc s v args s1
s_ds (Call v  params) s = callProc s v params s_ds

data ReplExpr = Aexp Aexp | Bexp Bexp deriving Show
data ReplStm = Stms [Stm] (Maybe ReplExpr) deriving Show

replExpr :: Parser ReplExpr
replExpr = Bexp <$> bexp
       <|> Aexp <$> aexp

replStm :: Parser ReplStm
replStm = Stms <$> stms <*> optional (replExpr)
      <|> flip (Stms . (:[])) Nothing <$> unsemidStm
      <|> Stms [] . Just <$> replExpr

getStm :: IO ReplStm
getStm = do
  hSetBuffering stdout NoBuffering
  getStmRec ""
  where
    getStmRec :: String -> IO ReplStm
    getStmRec pred = do
      putStr $ if null pred
               then "while> "
               else " ... > "
      done <- isEOF
      when done exitSuccess
      this <- getLine
      let line = pred ++ this
      case parsePartial (whitespace *> replStm) line of
        SIncomplete -> getStmRec (line ++ "\n")
        SFailed -> hPutStrLn stderr "Syntax error" >> getStm
        SParsed s -> pure s

doOrLog :: MonadIO m => Either String a -> (a -> m ()) -> m ()
x `doOrLog` f = either (liftIO . hPutStrLn stderr) f x

replT :: StateT Stack IO ()
replT = do
  liftIO (putStrLn "While evaluator, v0.1 (May 2018) by Charlie Harding")
  forever $ do
  Stms ss mExp <- liftIO getStm
  stack <- get
  let 
    liftPrint :: (Show a) => a -> StateT s IO ()
    liftPrint = liftIO . print
    showBool :: Bool -> String
    showBool True = "true"
    showBool False = "false"

  case foldM (flip s_ds) stack ss of
    Left error -> liftIO $ hPutStrLn stderr error
    Right stack' -> do
      put stack'
      mExp `forM_` \case
        Aexp n -> a_val n stack' `doOrLog` (liftIO . print)
        Bexp b -> b_val b stack' `doOrLog` (liftIO . putStrLn . showBool)

printOutput :: Stack -> IO ()
printOutput s = [0..fromMaybe 0 (lookup s "$") - 1] `forM_` \i ->
  maybe (putStrLn "undefined") print $ lookup s ("$;" ++ show i)

runFile :: Bool -> String -> [String] -> IO ()
runFile debug file args = do
  let entries = ("$", length args) : zip (map (("$;"++) . show)[0..]) (map read args :: [Int])
  let state = foldl (uncurry . update) emptyStack entries
  case parse (whitespace *> stms) file of
    Nothing -> hPutStrLn stderr "Syntax Error"
    Just ss -> do
      when debug $ putStrLn (intercalate "\n" $ map show ss) >> putStrLn ""
      s_ds (Block ss) state `doOrLog` printOutput

main = do
  args <- getArgs
  case args of
    [] -> evalStateT replT emptyStack
    "-d" : path : xs -> do
      tryIOError (readFile path) >>= \case
        Left except -> hPutStrLn stderr $ "While: file not found: "++path
        Right file -> runFile True file xs
    path : xs -> do
      tryIOError (readFile path) >>= \case
        Left except -> hPutStrLn stderr $ "While: file not found: "++path
        Right file -> runFile False file xs