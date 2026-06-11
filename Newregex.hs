{-# LANGUAGE LambdaCase #-}
module Newregex where
import Control.Monad.State
import Control.Monad.Except
import Data.List
import Data.Bifunctor
import Control.Monad
import Data.Functor
import Data.Char
import Data.Foldable
import Data.Either

-- Parser

type Parser a = StateT String (Except String) a

runParser :: Parser a -> String -> Either String (a, String)
runParser p s = runExcept (runStateT p s)

evalParser :: Parser a -> String -> Either String a
evalParser p s = second fst (runParser p s)

(<|>) :: MonadError e m => m a -> m a -> m a
f <|> g = catchError f (const g)
infixl 3 <|>

optional :: MonadError e m => m a -> m (Maybe a)
optional f = Just <$> f <|> pure Nothing

many :: MonadError e m => m a -> m [a]
many p = some p <|> pure []

some :: MonadError e m => m a -> m [a]
some p = (:) <$> p <*> many p

-- Primitívek

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

sepBy1 :: Parser a -> Parser delim -> Parser {- nem üres -} [a]
sepBy1 p delim = (:) <$> (p <|> throwError "sepBy1: no elements")
                     <*> ((delim *> sepBy p delim) <|> pure [])

sepBy :: Parser a -> Parser delim -> Parser [a]
sepBy p delim = sepBy1 p delim <|> pure []

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


-- A következő regexek támogatottak:
data RegEx
  -- Atomok:
  -- - (p) : (nincs külön konstruktora,
  --         hiszen a zárójelek nem jelennek meg az absztrakt szintaxisfában)
  -- - a : Karakter literál, amely betű, szóköz vagy szám lehet
  = REChar Char
  -- - [c1-c2] : Két karakter által meghatározott (mindkét oldalról zárt) intervallum
  --             Példák: [a-z], [0-9], ...
  | RERange Char Char
  -- - . : Tetszőleges karakter
  | REAny
  -- - $ : Üres bemenet ("End of file")
  | REEof

  -- Posztfix operátorok:
  -- - p* : Nulla vagy több ismétlés
  | REMany RegEx
  -- - p+ : Egy vagy több ismétlés
  | RESome RegEx
  -- - p? : Nulla vagy egy előfordulás
  | REOptional RegEx
  -- - p{n} : N-szeres ismétlés
  | RERepeat RegEx Int

  -- Infix operátorok:
  -- - Regex-ek egymás után futtatása.
  --   Jobbra asszociáló infix művelet, a szintaxisban "láthatatlan", egyszerűen
  --   egymás után írunk több regexet.
  | RESequence RegEx RegEx
  -- - p1|p2 : Először p1 futtatása, ha az nem illeszkedik, akkor p2.
  -- - Jobbra asszociál.
  | REChoice RegEx RegEx
  deriving (Eq, Show)

parseAtom :: Parser RegEx
parseAtom = 
    ( do
      char '('
      regex <- pRegEx
      char ')'
      pure regex  
    )
    <|>
    (do
        char '['
        a <- anyChar
        char '-'
        b <- anyChar
        char ']'
        pure $ RERange a b
    ) <|>
    (do
        char '.'
        pure REAny
    ) <|>
    (do
        char '$'
        pure REEof      
    ) <|>
    (do
        a <- satisfy (`notElem` "()[]|*+?{}$")
        pure $ REChar a
    ) 

parsePostFix :: Parser RegEx
parsePostFix = do
    v <- parseAtom
    parsePostHelp v
    where 
        parsePostHelp regex = 
            (do
            char '*'
            parsePostHelp (REMany regex)
            ) <|> (do
            char '+'
            parsePostHelp (RESome regex)
            ) <|> (do
            char '?'
            parsePostHelp (REOptional regex)
            ) <|> (do
            char '{'
            a <- integer 
            char '}'
            parsePostHelp (RERepeat regex a)
            ) <|> pure regex
parseSequence :: Parser RegEx
parseSequence = do
    v <- parsePostFix
    parseHelp v
    where 
        parseHelp v =
            (do 
                next <- parseSequence
                parseHelp (RESequence v next)
            ) <|> pure v


parseReChoice :: Parser RegEx
parseReChoice = rightAssoc REChoice parseSequence (char '|')

pRegEx :: Parser RegEx
pRegEx = parseReChoice

manyReg :: RegEx -> Parser ()
manyReg regex = (makeParser regex >> manyReg regex) <|> return ()

someReg :: RegEx -> Parser ()
someReg regex = (makeParser regex >> manyReg regex) <|> throwError ""

repeatReg :: RegEx -> Int -> Parser ()
repeatReg regex count = 
    if (count == 0) then return ()
    else (makeParser regex) >> (repeatReg regex (count-1))

makeParser :: RegEx -> Parser ()
makeParser (REChoice regex1 regex2) = makeParser regex1 <|> makeParser regex2 
makeParser (RESequence regex1 regex2) = makeParser regex1 >> makeParser regex2
makeParser (REMany regex) = manyReg regex
makeParser (RESome regex) = someReg regex
makeParser (REOptional regex) = makeParser regex <|> return ()
makeParser (RERepeat regex count) = repeatReg regex count
makeParser (REChar c) = char c
makeParser (RERange left right) = do
    parseChar <- anyChar
    when (parseChar < left || parseChar > right) $ throwError ""
makeParser (REAny) = void anyChar
makeParser (REEof) = do
    str <- get
    case str of
        [] -> return ()
        (x:xs) -> throwError ""

test :: String -> String -> Either String Bool
test pattern input = do
  regEx <- evalParser pRegEx pattern
  return (isRight (evalParser (makeParser regEx) input))

test' :: String -> String -> Bool
test' regex str = case test regex str of
  Left e  -> error e
  Right b -> b

licensePlate = "[A-Z]{3}[0-9]{3}$"
hexColor = "0x([0-9]|[A-F]){6}$"

-- regex101.com/r/rkScYV
-- regexr.com/5rrhl
streetName = "([A-Z][a-z]* )+(utca|út) [0-9]+([A-Z])?"

tests' :: [Bool]
tests' =
  [       test' licensePlate "ABC123"
  ,       test' licensePlate "IRF764"
  ,       test' licensePlate "LGM859"
  ,       test' licensePlate "ASD789"
  , not $ test' licensePlate "ABCD1234"
  , not $ test' licensePlate "ABC123asdf"
  , not $ test' licensePlate "123ABC"
  , not $ test' licensePlate "asdf"

  --

  ,       test' hexColor "0x000000"
  ,       test' hexColor "0x33FE67"
  ,       test' hexColor "0xFA55B8"
  , not $ test' hexColor "1337AB"
  , not $ test' hexColor "0x1234567"
  , not $ test' hexColor "0xAA1Q34"

  --

  ,       test' streetName "Ady Endre út 47C"
  ,       test' streetName "Karinthy Frigyes út 8"
  ,       test' streetName "Budafoki út 3"
  ,       test' streetName "Szilva utca 21A"
  ,       test' streetName "Nagy Lantos Andor utca 9"
  ,       test' streetName "T utca 1"
  , not $ test' streetName "ady Endre út 47C"
  , not $ test' streetName "KarinthyFrigyes út 8"
  , not $ test' streetName "út 3"
  , not $ test' streetName "Liget köz 21A"
  , not $ test' streetName "Nagy  Lantos  Andor utca 9"
  , not $ test' streetName "T utca"
  ]