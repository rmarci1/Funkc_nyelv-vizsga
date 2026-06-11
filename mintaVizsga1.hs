{-# LANGUAGE LambdaCase #-}
import Data.List
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.List
import Data.Bifunctor
import Control.Monad
import Data.Functor
import Data.Char
import Data.Foldable
import Data.Bitraversable

data LengthIndexedList i a = Nil | Cons i a (LengthIndexedList i a) deriving (Eq, Show)

instance Functor (LengthIndexedList fixed) where
    fmap :: (a -> b) -> LengthIndexedList fixed a -> LengthIndexedList fixed b
    fmap f Nil = Nil
    fmap f (Cons fixed a b) = Cons fixed (f a) (fmap f b)

instance Foldable (LengthIndexedList fixed) where
    foldr :: (a -> b -> b) -> b -> LengthIndexedList fixed a -> b
    foldr f b Nil = b
    foldr f b (Cons _ a x) = f a (foldr f b x)  

    foldMap :: Monoid m => (a -> m) -> LengthIndexedList fixed a -> m
    foldMap f Nil = mempty
    foldMap f (Cons _ a x) = f a <> foldMap f x
instance Traversable (LengthIndexedList fixed) where
    traverse :: (Applicative f) => (a -> f b) -> LengthIndexedList fixed a -> f (LengthIndexedList fixed b)
    traverse f Nil = pure Nil
    traverse f (Cons fixed a b) = Cons <$> pure fixed <*> f a <*> traverse f b  

l1 :: LengthIndexedList Integer Char
l1 = Cons 11 'h' $ Cons 10 'e' $ Cons 9 'l' $ Cons 8 'l' $ Cons 7 'o' $ Cons 6 ' ' $ Cons 5 'w' $ Cons 4 'o' $ Cons 3 'r' $ Cons 2 'l' $ Cons 1 'd' $ Nil

l1test :: LengthIndexedList Integer Char
l1test = Cons 5 'h' $ Cons 4 'e' $ Cons 3 'l' $ Cons 2 'l' $ Cons 1 'o' $ Nil

l2 :: LengthIndexedList Int Bool
l2 = let l@(Cons i a r) = fmap not l2 in Cons (i + 1) True l

l3 :: LengthIndexedList Integer Int
l3 = Cons 2 2 $ Cons 1 1 Nil

satisfyInvariant :: (Num i, Eq i) => LengthIndexedList i a -> Bool
satisfyInvariant Nil = True
satisfyInvariant (Cons index a next) = satisfyHelp (index-1) next && satisfyInvariant next
    where
        satisfyHelp ind Nil = ind == 0
        satisfyHelp ind (Cons _ _ next) = satisfyHelp (ind-1) next 

mkLIL :: (Foldable f, Num i) => f a -> LengthIndexedList i a
mkLIL xs = go (foldr (:) [] xs)
  where
    go [] = Nil
    go xs = build (length xs) xs

    build _ [] = Nil
    build n (x:xs) =
        Cons (fromIntegral n) x (build (n-1) xs)

reverseLIL :: Num i => LengthIndexedList i a -> LengthIndexedList i a
reverseLIL a = mkLIL $ reverse $ reverseLILHelp a

reverseLILHelp :: LengthIndexedList i a -> [a]
reverseLILHelp Nil = []
reverseLILHelp (Cons _ a next) = a : (reverseLILHelp next)

type Lawnmover a = StateT ((Int,Int) -> Bool) (WriterT [(Int, Int)] IO) a

runLawnmover lawnmoverMonad initialState = runWriterT $ runStateT lawnmoverMonad initialState

mowAt :: (Int,Int) -> Lawnmover Bool
mowAt coordinate = do
    state <- get
    if (state coordinate) then do
        put (\x -> coordinate /= x && state x)
        tell [coordinate]
        return True
    else return False

drawLawn :: (Int,Int) -> (Int,Int) -> Lawnmover ()
drawLawn (x,y) (x2,y2) = do
    drawHelp (x, y) (x2, y2)
    if (x == x2) then return ()
    else do 
        liftIO $ putStrLn ""
        drawLawn ((x+1),y) (x2,y2)

drawHelp :: (Int,Int) -> (Int,Int) -> Lawnmover ()
drawHelp (x,y) (x2,y2) 
    | y > y2 = return ()
    | otherwise = do
        mowed <- mowAt (x,y)
        if not mowed then liftIO $ putStr "# " 
        else liftIO $ putStr "* "
        drawHelp (x, (y+1)) (x2, y2)






-- Parser

type Parser a = StateT String (Except String) a

runParser :: Parser a -> String -> Either String (a, String)
runParser p s = runExcept (runStateT p s)

(<|>) :: MonadError e m => m a -> m a -> m a
f <|> g = catchError f (const g)
infixl 3 <|>

optional :: MonadError e m => m a -> m (Maybe a)
optional f = Just <$> f <|> pure Nothing

-- Run parser 0 or more times
many :: MonadError e m => m a -> m [a]
many p = some p <|> pure []

-- Run parser 1 or more times
some :: MonadError e m => m a -> m [a]
some p = (:) <$> p <*> many p

-- Primitive parser combinators

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = get >>= \case
  (c:cs) | p c -> c <$ put cs
  _            -> throwError "satisfy: condition not met or string empty"

eof :: Parser ()
eof = get >>= (<|> throwError "eof: String not empty") . guard . null

char :: Char -> Parser ()
char c = void $ satisfy (== c) <|> throwError ("char: not equal to " ++ [c])

anyChar :: Parser Char
anyChar = satisfy (const True)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit <|> throwError "digit: Not a digit"

string :: String -> Parser ()
string str = mapM_ (\c -> char c <|> throwError ("string: mismatch on char " ++ [c] ++ " in " ++ str)) str

between :: Parser left -> Parser a -> Parser right -> Parser a
between l a r = l *> a <* r

natural :: Parser Int
natural = foldl1 (\acc a -> acc * 10 + a) <$> (some (digitToInt <$> satisfy isDigit) <|> throwError "natural: number had no digits")

integer :: Parser Int
integer = maybe id (const negate) <$> optional (char '-') <*> natural

float :: Parser Double
float = do
    s <- maybe id (const negate) <$> optional (char '-')
    i <- natural
    char '.' <|> throwError "float: No digit separator"
    r <- foldr1 (\a acc -> a + acc / 10) <$> some (fromIntegral <$> digit)
    pure $ s (r / 10 + fromIntegral i)

sepBy1 :: Parser a -> Parser delim -> Parser {- not empty -} [a]
sepBy1 p delim = (:) <$> (p <|> throwError "sepBy1: no elements")
                     <*> ((delim *> sepBy p delim) <|> pure [])

sepBy :: Parser a -> Parser delim -> Parser [a]
sepBy p delim = sepBy1 p delim <|> pure []

-- Whitespace dropping
ws :: Parser ()
ws = void $ many $ satisfy isSpace

-- Tokenisation: dropping all whitespaces after a parser
tok :: Parser a -> Parser a
tok p = p <* ws

topLevel :: Parser a -> Parser a
topLevel p = ws *> tok p <* eof

-- We label tokenized parsers with '

natural' :: Parser Int
natural' = tok natural

integer' :: Parser Int
integer' = tok integer

float' :: Parser Double
float' = tok float

char' :: Char -> Parser ()
char' c = tok $ char c

string' :: String -> Parser ()
string' str = tok $ string str

rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f p sep = chainr1 p (f <$ sep)

leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f p sep = chainl1 p (f <$ sep)

nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc f pa psep = do
  exps <- sepBy1 pa psep
  case exps of
    [e] -> pure e
    [e1, e2] -> pure (f e1 e2)
    _ -> throwError "nonAssoc: too many or too few associations"

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 v op = do
  val <- v
  ( do
      opr <- op
      res <- chainr1 v op
      pure (opr val res)
    )
    <|> pure val

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 v op = v >>= parseLeft
  where
    parseLeft val =
      ( do
          opr <- op
          val2 <- v
          parseLeft (opr val val2)
      )
        <|> pure val

-- Expression language
data Exp
  = IntLit Int           -- 1 2 ...
  | FloatLit Double      -- 1.0 2.11 ...
  | BoolLit Bool         -- true false
  | Var String           -- x y ...
  | LamLit String Exp    -- \x -> e
  | ListLit [Exp]
  | Exp :+ Exp           -- e1 + e2
  | Exp :* Exp           -- e1 * e2
  | Exp :- Exp           -- e1 - e2
  | Exp :/ Exp           -- e1 / e2
  | Exp :== Exp          -- e1 == e2
  | Exp :$ Exp           -- e1 $ e2
  | Exp :!! Exp
  | Not Exp              -- not e
  deriving (Eq, Show)

{-
+--------------------+--------------------+--------------------+
| Operator name      | Direction          | Precedence         |
+--------------------+--------------------+--------------------+
| not                | Prefix             | 20                 |
+--------------------+--------------------+--------------------+
| *                  | Right              | 18                 |
+--------------------+--------------------+--------------------+
| /                  | Left               | 16                 |
+--------------------+--------------------+--------------------+
| +                  | Right              | 14                 |
+--------------------+--------------------+--------------------+
| -                  | Left               | 12                 |
+--------------------+--------------------+--------------------+
| ==                 | None               | 10                 |
+--------------------+--------------------+--------------------+
| $                  | Right              | 8                  |
+--------------------+--------------------+--------------------+

-}

keywords :: [String]
keywords = ["true", "false", "not"]

pNonKeyword :: Parser String
pNonKeyword = do
  res <- tok $ some (satisfy isLetter)
  res <$ (guard (res `notElem` keywords) <|> throwError "pNonKeyword: parsed a keyword")

pKeyword :: String -> Parser ()
pKeyword = string'

listListSplit :: Parser [Exp]
listListSplit = do
  char' ','
  p <- pExp
  (
    do
      rest <- listListSplit
      return $ p : rest 
    ) <|> return [p]

listLitHelp :: Parser [Exp]
listLitHelp = do
  char' '['
  p <- pExp
  (
    do 
      rest <- listListSplit
      char' ']'
      return $ p : rest
   ) <|> do 
    char' ']'
    return [p]

pAtom :: Parser Exp
pAtom = asum [
  FloatLit <$> float',
  IntLit <$> integer',
  BoolLit True <$ pKeyword "true",
  BoolLit False <$ pKeyword "false",
  LamLit <$> (pKeyword "lam" *> pNonKeyword) <*> (string' "->" *> pExp),
  Var <$> pNonKeyword,
  ListLit <$> listLitHelp,
  between (char' '(') pExp (char' ')')
             ] <|> throwError "pAtom: no literal, var or bracketed matches"

pNot :: Parser Exp
pNot = (Not <$> (pKeyword "not" *> pNot)) <|> pAtom

pMul :: Parser Exp
pMul = chainr1 pNot ((:*) <$ char' '*')

pDiv :: Parser Exp
pDiv =  chainl1 pMul ((:!!) <$ string' "!!") <|> chainl1 pMul ((:/) <$ char' '/')

pAdd :: Parser Exp
pAdd = chainr1 pDiv ((:+) <$ char' '+')

pMinus :: Parser Exp
pMinus = chainl1 pAdd ((:-) <$ char' '-')

pEq :: Parser Exp
pEq = nonAssoc (:==) pMinus (string' "==")

pDollar :: Parser Exp
pDollar = chainr1 pEq ((:$) <$ char' '$')

pExp :: Parser Exp -- bottom of the table
pExp = pDollar

-- Statements: assigment, branching, loops
data Statement
  = If Exp [Statement]        -- if e then p end
  | While Exp [Statement]     -- while e do p end
  | Assign String Exp         -- v := e
  | AssignAt String Exp Exp
  | Index String Exp
  deriving (Eq, Show)

-- Define parser for the statements above!
-- All statements in a program must be followed by a semicolon

program :: Parser [Statement]
program = sepBy1 statement (char' ';')

statement :: Parser Statement
statement = sIf <|> sWhile <|> sAssign <|> sAssignAt <|> sIndex

-- Alternative:
-- program = some statement
-- statement = (sIf <|> sWhile <|> sAssign) <* (char' ';')

sIf :: Parser Statement
sIf = do
  pKeyword "if"
  exp <- pExp
  pKeyword "then"
  stmts <- program 
  pKeyword "end"
  return $ If exp stmts

sWhile :: Parser Statement
sWhile = do
  pKeyword "while"
  exp <- pExp 
  pKeyword "do"
  stmts <- program 
  pKeyword "end"
  return $ While exp stmts

sAssign :: Parser Statement
sAssign = do
  varname <- pNonKeyword
  pKeyword ":="
  exp <- pExp 
  return $ Assign varname exp

sAssignAt :: Parser Statement
sAssignAt = do
  varname <- pNonKeyword
  pKeyword "!!"
  exp <- pExp 
  pKeyword ":="
  exp2 <- pExp 
  return $ AssignAt varname exp exp2

sIndex :: Parser Statement
sIndex = do
  varname <- pNonKeyword
  pKeyword "!!"
  exp <- pExp
  return $ Index varname exp

parseProgram :: String -> Either String [Statement]
parseProgram s = case runParser (topLevel program) s of
  Left e -> Left e
  Right (x,_) -> Right x

-- Interpreter
-- Type of evaluated expressions:
data Val
  = VInt Int              -- evaled int
  | VFloat Double         -- evaled double
  | VBool Bool            -- evaled bool
  | VLam String Env Exp   -- evaled lam, Env is the environment at the point the lambda was created (the closure)
  | VList [Val]
  deriving (Eq, Show)

type Env = [(String, Val)] -- the evaluation environment

data InterpreterError
  = TypeError {msg :: String} -- type errors 
  | ScopeError {msg :: String} -- scope errors
  | DivByZeroError {msg :: String} -- division by zero errors
  | IndexOutOfRangeError Int [Val] 
  deriving (Eq, Show)

-- We don't explicitly give the type of the interpreter monad, but use constraints instead
-- Let's evaluate expressions!
-- Note: evalExp cannot modify the Env
helpSearch :: MonadError InterpreterError m => [Val] -> m Bool
helpSearch (x : y : xs) = 
  case (x,y) of
    (VInt _, VInt _) -> helpSearch $ y : xs
    (VFloat _, VFloat _) -> helpSearch $ y : xs
    (VBool _, VBool _) -> helpSearch $ y : xs
    (VLam _ _ _, VLam _ _ _) -> helpSearch $ y : xs
    (VList _, VList _) -> helpSearch $ y : xs
    _ -> return False
helpSearch _ = return True

checkingEquals :: MonadError InterpreterError m => [Val] -> [Val] -> m Bool
checkingEquals [] [] = return True
checkingEquals (x:xs) (y:ys)
      | x == y = checkingEquals xs ys
      | otherwise = return False
checkingEquals _ _ = return False

help :: MonadError InterpreterError m => [Exp] -> Env -> m [Val]
help [] _ = return []
help (x:xs) env = do
  p <- evalExp x env
  rest <- help xs env
  return $ p:rest
evalExp :: MonadError InterpreterError m => Exp -> Env -> m Val
evalExp e env = case e of 
  IntLit x -> return $ VInt x
  FloatLit x -> return $ VFloat x
  BoolLit x -> return $ VBool x 
  Var str -> case lookup str env of
    Nothing -> throwError $ ScopeError ("Variable " ++ str ++ " not in scope")
    Just val -> return val
  LamLit str exp -> return $ VLam str env exp  
  ListLit exp -> 
    case exp of
      [] -> return $ VList []
      xs -> do
        rest <- help xs env
        bool <- helpSearch rest
        if bool then return $ VList rest
        else throwError $ TypeError "Different types of values in a list"
  x :+ y -> do
    xv <- evalExp x env
    yv <- evalExp y env
    case (xv, yv) of
      (VInt xx, VInt yy) -> return $ VInt (xx + yy)
      (VFloat xx, VFloat yy) -> return $ VFloat (xx + yy)
      (VList xx, VList yy) -> return $ VList $ xx ++ yy
      _ -> throwError $ TypeError "Adding values of different types"
  x :* y -> do
    xv <- evalExp x env 
    yv <- evalExp y env
    case (xv, yv) of 
      (VInt xx, VInt yy) -> return $ VInt (xx * yy)
      (VFloat xx, VFloat yy) -> return $ VFloat (xx * yy)
      _ -> throwError $ TypeError "Multiplying values of different types"
  x :- y -> do
    xv <- evalExp x env 
    yv <- evalExp y env
    case (xv, yv) of 
      (VInt xx, VInt yy) -> return $ VInt (xx - yy)
      (VFloat xx, VFloat yy) -> return $ VFloat (xx - yy)
      _ -> throwError $ TypeError "Subtracting values of different types"
  x :/ y -> do 
    xv <- evalExp x env 
    yv <- evalExp y env
    case (xv, yv) of 
      (VInt xx, VInt 0) -> throwError $ DivByZeroError "Dividing by 0"
      (VInt xx, VInt yy) -> return $ VInt (xx `div` yy)
      -- (VFloat xx, VFloat yy) -> return $ VFloat (xx / yy)
      _ -> throwError $ TypeError "Dividing values of different types"
  x :== y -> do 
    xv <- evalExp x env
    yv <- evalExp y env 
    case (xv, yv) of 
      (VInt xx, VInt yy) -> return $ VBool (xx == yy)   
      (VFloat xx, VFloat yy) -> return $ VBool (xx == yy)
      (VBool xx, VBool yy) -> return $ VBool (xx == yy)
      (VList xx, VList yy) ->  do
        bool <- checkingEquals xx yy
        return $ VBool bool
      _ -> throwError $ TypeError "Checking equality on different types"     
  x :$ y -> do
    xv <- evalExp x env 
    yv <- evalExp y env 
    case xv of 
      VLam str env' exp -> evalExp exp ((str, yv) : env')
      _ -> throwError $ TypeError "Application not on a lambda"
      -- ex. (\z -> z + x + 2) :$ 7 [(x, 5)]
  x :!! y -> do
    xv <- evalExp x env 
    yv <- evalExp y env 
    case (xv,yv) of
      (VList xs, VInt yy) -> do
          if (yy < 0 || (yy > ((length xs) - 1))) then throwError $ IndexOutOfRangeError yy xs
          else return $ xs !! yy
      _ -> throwError $ TypeError "Checking index on wrong type"     
  Not x -> do 
    xv <- evalExp x env 
    case xv of 
      VBool b -> return $ VBool (not b)
      _ -> throwError $ TypeError "Negation not on a Bool"      

testEvalExp :: String -> Either InterpreterError Val
testEvalExp s = case runParser (topLevel pExp) s of
  Left _ ->  throwError (TypeError "Couldnt parse whole string")
  Right (e, _) -> runExcept (evalExp e [])

-- Try running:
-- testEvalExp "lam x -> 3 + x"

-- We store the environment inside a state monad
-- Let's evaluate statements!
-- Note: evalStatement can modify the Env
evalStatement :: (MonadError InterpreterError m, MonadState Env m) => 
                 Statement -> m ()
evalStatement (If e stmts) = do 
  env <- get 
  cond <- evalExp e env 
  case cond of 
    VBool True -> inBlockScope $ evalProgram stmts
    VBool False -> return ()
    _ -> throwError $ TypeError "If condition not a Bool"
evalStatement (While e stmts) = do
  env <- get 
  cond <- evalExp e env 
  case cond of 
    VBool True -> do 
      evalProgram stmts
      evalStatement (While e stmts)
    VBool False -> return ()
    _ -> throwError $ TypeError "While condition not a Bool"

evalStatement (Assign var e) = do 
  env <- get 
  exp <- evalExp e env 
  put $ updateEnv var exp env -- modify (\env' -> updateEnv var exp env')
  
evalStatement (AssignAt str exp1 exp2) = do
  env <- get
  val1 <- evalExp exp1 env
  val2 <- evalExp exp2 env
  put $ updateEnv' str val1 val2 env 

-- Auxiliary functions inBlockScope and updateEnv:

-- Get the length of the env,
-- perform the operation, 
-- modify the env to be the length that it was originally, 
-- and return the result of the operation
inBlockScope :: MonadState Env m => m a -> m a
inBlockScope m = do 
  l <- length <$> get 
  ret <- m
  modify (take l)
  return ret

updateHelp :: Int -> Int -> Val -> Val -> Val
updateHelp curr toind toval (VList []) = VList []
updateHelp curr toind toval (VList (x:xs)) 
    | curr == toind = VList $ toval : xs
    | otherwise = 
      case (updateHelp (curr+1) toind toval (VList xs)) of
        (VList ys) -> VList $ x:ys
        _ -> VList $ x:xs
updateHelp _ _ _ x = x


updateEnv' :: String -> Val -> Val -> Env -> Env
updateEnv' s _ val [] = [(s, val)]
updateEnv' s (VInt a) val ((s', val') : xs) 
  | s == s'   = (s', (updateHelp 0 a val val')) : xs
  | otherwise = (s', val') : updateEnv' s (VInt a) val xs
-- If env contains variable name, update in place
-- If not, add it TO THE END of the env
updateEnv :: String -> Val -> Env -> Env
updateEnv s val [] = [(s, val)]
updateEnv s val ((s', val') : xs) 
  | s == s'   = (s, val) : xs
  | otherwise = (s', val') : updateEnv s val xs

evalProgram :: (MonadError InterpreterError m, MonadState Env m) => 
               [Statement] -> m ()
evalProgram = mapM_ evalStatement

runProgramT :: Monad m => [Statement] -> m (Either InterpreterError Env)
runProgramT = runExceptT . flip execStateT [] . evalProgram

runProgram :: [Statement] -> Either InterpreterError Env
runProgram = runExcept . flip execStateT [] . evalProgram

runProgramPretty :: [Statement] -> IO ()
runProgramPretty sts = do
  res <- runProgramT sts
  case res of
    Right env -> forM_ env $ \(var, val) -> putStrLn $ var ++ " == " ++ show val
    Left err -> putStrLn (msg err)

parseAndRunProgram :: String -> IO ()
parseAndRunProgram s = do
  Right r <- bitraverse fail pure (parseProgram s)
  runProgramPretty r