{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module While where

import            Prelude                   hiding (lookup)
import            Control.Applicative       (Applicative, Alternative, empty, (<|>), some, many)
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
import            Data.Map.Lazy             (Map, member, insert, alter, (!?))
import qualified  Data.Map.Lazy             as Map
import            Parser
import            Parser.Result             (SingleResult(..))

data LVal
  = Arr LVal Aexp
  | Var Var
  deriving Show

data Aexp
  = Num Int
  | LVal LVal
  | Aexp :+: Aexp
  | Aexp :*: Aexp
  | Aexp :-: Aexp
  deriving Show

data Bexp
  = T
  | F
  | Aexp :=: Aexp
  | Aexp :<: Aexp
  | Aexp :>: Aexp
  | Bexp :&: Bexp
  | Not Bexp
  deriving Show

type Var = String
type Args = ([Var], [(Var, Aexp)], Maybe Var)
type VarParam = Maybe (LVal, Aexp)
type Params = ([Aexp], VarParam)

data Stm
  = LVal := Aexp
  | Proc Var Args Stm
  | Skip
  | Stm :> Stm
  | If Bexp Stm Stm
  | While Bexp Stm
  | Block [(Var, Aexp)] Stm
  | Call Var Params
  deriving Show

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
bexp = precedence [(|:) <$ tok "|"
                  ,(:&:) <$ tok "&"]
      $ T <$ word "true"
    <|> F <$ word "false"
    <|> (:=:) <$> aexp <* tok "==" <*> aexp
    <|>  neq  <$> aexp <* tok "!=" <*> aexp
    <|>  lte  <$> aexp <* tok "<=" <*> aexp
    <|> (:<:) <$> aexp <* tok "<" <*> aexp
    <|>  gte  <$> aexp <* tok ">=" <*> aexp
    <|> (:>:) <$> aexp <* tok ">" <*> aexp
    <|> Not <$ tok "!" <*> bexp
    <|> tok "(" *> bexp <* tok ")"

neq :: Aexp -> Aexp -> Bexp
x `neq` y = Not (x :=: y)

lte :: Aexp -> Aexp -> Bexp
x `lte` y = Not (x :>: y)

gte :: Aexp -> Aexp -> Bexp
x `gte` y = Not (x :<: y)

(|:) :: Bexp -> Bexp -> Bexp
x |: y = Not (Not x :&: Not y)

arrayRoot :: Var -> Var
arrayRoot = takeWhile (/= ';')

vars :: Parser [(Var, Aexp)]
vars = many $ (,) <$ word "var" <*> var <* tok "=" <*> aexp <* tok ";"

optional :: Parser a -> Parser (Maybe a)
optional p = Just <$> p <|> pure Nothing

stms :: Parser Stm
stms = foldl (:>) Skip <$> many semidStm

unsemidStm :: Parser Stm
unsemidStm = simpleStm <|> compoundStm

semidStm :: Parser Stm
semidStm = simpleStm <* tok ";" <|> compoundStm <|> Skip <$ tok ";"

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

compoundStm :: Parser Stm
compoundStm = If <$ word "if" <*> bexp <* optional (tok ";")
                 <* word "then" <*> unsemidStm
                 <* word "else" <*> semidStm
          <|> While <$> loopCond <* word "do" <*> semidStm
          <|> Block <$ tok "{" <*> vars <*> stms <* tok "}"
          <|> Proc  <$ word "proc" <*> var <*> args <*> semidStm

simpleStm :: Parser Stm
simpleStm = (:=) <$> lval <* tok "=" <*> aexp
        <|> repeatWhile <$ word "repeat" <*> unsemidStm <*> loopCond
        <|> modAssn <$> lval <*> foldr (<|>) empty aops <* tok "=" <*> aexp
        <|> Skip <$ word "skip"
        <|> Call <$> var <*> params

modAssn :: LVal -> (Aexp -> Aexp -> Aexp) -> Aexp -> Stm
modAssn var op val = var := (LVal var `op` val)

repeatWhile :: Stm -> Bexp -> Stm
repeatWhile s cond = s :> While cond s

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

data Function = Args :-> Stm

type State = [(Map Var Int, Map Var Function)]

lookup :: State -> Var -> Maybe Int
lookup [] _ = Nothing
lookup (s:ss) var
  | member (arrayRoot var) (fst s) = fst s !? var
  | otherwise = lookup ss var

pushState :: [(Var, Aexp)] -> State -> Either String State
pushState vars state = foldM pushVar ((Map.empty, Map.empty) : state) vars
  where
    pushVar :: State -> (Var, Aexp) -> Either String State
    pushVar ((vs,ps):ss) (v, x) = (\x' -> (insert v x' vs, ps) : ss) <$> a_val x ((vs,ps):ss)

popState :: State -> State
popState (s:ss) = ss

var_name :: LVal -> State -> Either String Var
var_name (Var v) s = Right v
var_name (Arr v i) s = do
  name <- var_name v s
  index <- a_val i s
  return $ name ++ ";" ++ show index

a_val :: Aexp -> State -> Either String Int
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

b_val :: Bexp -> State -> Either String Bool
b_val T s = Right True
b_val F s = Right False
b_val (Not b) s = not <$> b_val b s
b_val (a1 :=: a2) s = liftM2 (==) (a_val a1 s) (a_val a2 s)
b_val (a1 :<: a2) s = liftM2 (<)  (a_val a1 s) (a_val a2 s)
b_val (a1 :>: a2) s = liftM2 (>)  (a_val a1 s) (a_val a2 s)
b_val (b1 :&: b2) s = liftM2 (&&) (b_val b1 s) (b_val b2 s)

update :: State -> Var -> Int -> State
update [(vs, ps)] v x = [(insert v x (alter (<|> Just 1) (arrayRoot v) vs), ps)]
update ((vs, ps):ss) v x
  | member (arrayRoot v) vs = (insert v x vs, ps) : ss
  | otherwise               = (vs, ps) : update ss v x

defProc :: State -> Var -> Args -> Stm -> State
defProc ((vs, ps):ss) v args s1 = (vs, insert v (args :-> s1) ps) : ss

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

assign :: Var -> Args -> Params -> State -> Either String (State, Stm)
assign function args params state = first ((: state) . (, Map.empty)) <$> (assignReqArgs 1 args =<< params')
  where
    params' :: Either String ([Int], Maybe (Var, Int))
    params' = bnidSnd getVarParam $ mapMFst (mapM (flip a_val state)) params

    getVarParam :: VarParam -> Either String (Maybe (Var, Int))
    getVarParam Nothing = Right Nothing
    getVarParam (Just (xs, n)) = (Just .) . (,) <$> var_name xs state <*> a_val n state


    assignReqArgs :: Int -> Args -> ([Int], Maybe (Var, Int)) -> Either String (Map Var Int, Stm)
    assignReqArgs i ([],os,v) p = assignOptArgs i (os,v) p
    assignReqArgs i (r:rs,os,v) ([],  vp) = Left $ "Missing argument #" ++ show i ++ " to function " ++ function ++ ": " ++ r
    assignReqArgs i (r:rs,os,v) (p:ps,vp) = do
      (vars, s1) <- assignReqArgs (succ i) (rs,os,v) (ps,vp)
      return (insert r p vars, s1)
    
    assignOptArgs :: Int -> ([(Var, Aexp)], Maybe Var) -> ([Int], Maybe (Var, Int)) -> Either String (Map Var Int, Stm)
    assignOptArgs i ([],v) (ps,vp) = assignVarArg (succ i) v (ps, vp)
    assignOptArgs i ((o,x):os,v) ([],vp) = second ((Var o := x) :>) <$> assignOptArgs (succ i) (os,v) ([],vp)
    assignOptArgs i ((o,x):os,v) (p:ps,vp) = first (insert o p) <$> assignOptArgs (succ i) (os,v) (ps, vp)

    assignVarArg :: Int -> Maybe Var -> ([Int], Maybe (Var, Int)) -> Either String (Map Var Int, Stm)
    assignVarArg i Nothing ([],Nothing) = assignmentBase
    assignVarArg i Nothing (p:ps,vp) = Left $ "Unexpected argument #" ++ show i ++ " to function " ++ function
    assignVarArg i Nothing ([],Just _) = Left $ "Unexpected vararg to function " ++ function
    assignVarArg i _ (p:ps,Just _) = Left $ "Unexpected argument #" ++ show i ++ " to function " ++ function
    assignVarArg i (Just v) ([],Just (vp, n)) = first (insert v n) <$> assignVarArgArray (n-1) v vp
    assignVarArg i (Just v) (vp, Nothing) = first (insert v (length vp) . insert (v ++ ";") 0) <$> assignVarArgInline 0 v vp

    assignVarArgArray :: Int -> Var -> Var -> Either String (Map Var Int, Stm)
    assignVarArgArray i v vp
      | i < 0     = assignmentBase
      | otherwise = assignVarArgArray (pred i) v vp `bindFst` \vars -> do
          x <- a_val (LVal (Arr (Var vp) (Num i))) state
          return (insert (v++";"++show i) x vars)
    
    assignVarArgInline :: Int -> Var -> [Int] -> Either String (Map Var Int, Stm)
    assignVarArgInline i v [] = assignmentBase
    assignVarArgInline i v (p:ps) = first (insert (v++";"++show i) p) <$> assignVarArgInline (succ i) v ps

    assignmentBase :: Either String (Map Var Int, Stm)
    assignmentBase = Right (Map.empty, Skip)

callProc :: State -> Var -> Params -> (Stm -> State -> Either String State) -> Either String State
callProc [] p _ _ = Left ("Function not found: "++p)
callProc s@((vs,ps):ss) p xs k = case ps !? p of
  Just (args :-> s1) -> popState <$> (assign p args xs s >>= \(state, bindings) -> k (bindings :> s1) state)
  Nothing            -> ((vs, ps):) <$> callProc ss p xs k

cond :: Monad m => (a->m Bool) -> (a->m b) -> (a->m b) -> (a->m b)
cond p t f x = p x >>= \b -> if b then t x else f x

fix :: (a -> a) -> a
fix f = let x = f x in x

s_ds :: Stm -> State -> Either String State
s_ds  Skip            s = Right s
s_ds (     v :=    x) s = update s <$> var_name v s <*> a_val x s
s_ds (s1     :>   s2) s = s_ds s2 =<< s_ds s1 s
s_ds (If     p s1 s2) s = cond (b_val p) (s_ds s1) (s_ds s2) s
s_ds (While  p    s1) s = fix (\f -> cond (b_val p) (f <=< s_ds s1) return) s
s_ds (Block vs    s1) s = popState <$> (s_ds s1 =<< pushState vs s)
s_ds (Proc v args s1) s = pure $ defProc s v args s1
s_ds (Call v  params) s = callProc s v params s_ds

emptyState :: State
emptyState = [(Map.empty, Map.empty)]

data ReplExpr = Aexp Aexp | Bexp Bexp deriving Show
data ReplStm = Stm Stm (Maybe ReplExpr) deriving Show

replExpr :: Parser ReplExpr
replExpr = Bexp <$> bexp
       <|> Aexp <$> aexp

replStm :: Parser ReplStm
replStm = Stm <$> stms <*> optional (replExpr)
      <|> flip Stm Nothing <$> unsemidStm
      <|> Stm Skip . Just <$> replExpr

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

replT :: StateT State IO ()
replT = do
  liftIO (putStrLn "While evaluator, v0.1 (May 2018) by Charlie Harding")
  forever $ do
  Stm s mExp <- liftIO getStm
  state <- get
  let 
    liftPrint :: (Show a) => a -> StateT s IO ()
    liftPrint = liftIO . print
    showBool :: Bool -> String
    showBool True = "true"
    showBool False = "false"

  case s_ds s state of
    Left error -> liftIO $ hPutStrLn stderr error
    Right state' -> do
      put state'
      mExp `forM_` \case
        Aexp n -> a_val n state' `doOrLog` (liftIO . print)
        Bexp b -> b_val b state' `doOrLog` (liftIO . putStrLn . showBool)

printOutput :: State -> IO ()
printOutput state = [0..fromMaybe 0 (lookup state "$") - 1] `forM_` \i ->
  maybe (putStrLn "undefined") print $ lookup state ("$;" ++ show i)

runFile :: String -> [String] -> IO ()
runFile file args = do
  let entries = ("$", length args) : zip (map (("$;"++) . show)[0..]) (map read args :: [Int])
  let state = foldl (uncurry . update) emptyState entries
  case parse (whitespace *> stms) file of
    Nothing -> hPutStrLn stderr "Syntax Error"
    Just s -> do
      s_ds s state `doOrLog` printOutput

main = do
  args <- getArgs
  case args of
    [] -> evalStateT replT emptyState
    path : xs -> do
      tryIOError (readFile path) >>= \case
        Left except -> hPutStrLn stderr $ "While: file not found: "++path
        Right file -> runFile file xs