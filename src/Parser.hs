{-# LANGUAGE LambdaCase #-}

module Parser
    where

import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad
import Data.Char
import Data.List

infixr 5 +++
newtype Parser a = P (String -> [(a, String)]) 

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap


instance Monad Parser where
    return v = P (\inp -> [(v,inp)])
    p >>= f = P (\inp ->
                     case parse p inp of
                       [(v, out)] -> parse (f v) out
                       [] -> [])

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

instance MonadPlus Parser where
    mzero                      =  P (const [])
    p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])

-- data Expr = Literal Double | Variable Var | Assign Var Expr | Add Expr Expr
-- newtype Var = Var String deriving (Ord, Eq, Show)

-- Basic parser
parse :: Parser a -> String -> [(a, String)]
parse (P p) = p 

failure :: Parser a
failure =  mzero

item :: Parser Char
item = P (\case 
              []     -> []
              (x:xs) -> [(x,xs)])

                                 
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q


sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

many' :: Parser a -> Parser [a]
many' p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many' p
             return (v:vs)

ident :: Parser String
ident = do x <- lower
           xs <- many' alphanum
           return (x:xs)

nat :: Parser Double
nat = do xs <- many1 digit
         return (read xs)

int :: Parser Double
int = (do char '-'
          n <- nat
          return (-n))
      +++ nat

pint :: Parser Double
pint = do n <- nat
          return n

double :: Parser (Double,String)
double = do char '-'
            i <- many1 digit
            char '.'
            j <- many1 digit
            char 'e'
            char '-'
            k <- many1 digit
            return (-(read $ i <> "." <> j <> "e-" <> k), "")
         <|> do i <- many1 digit
                char '.'
                j <- many1 digit
                char 'e'
                char '-'
                k <- many1 digit
                return (read $ i <> "." <> j <> "e-" <> k, "")
             <|> do char '-'
                    i <- many1 digit
                    char '.'
                    j <- many1 digit
                    char 'e'
                    k <- many1 digit
                    return (-(read $ i <> "." <> j <> "e" <> k), "")
                 <|> do i <- many1 digit
                        char '.'
                        j <- many1 digit
                        char 'e'
                        k <- many1 digit
                        return (read $ i <> "." <> j <> "e" <> k, "")
                     <|> do char '-'
                            i <- many1 digit
                            char 'e'
                            j <- many1 digit
                            return (-(read $ i <> "e" <> j), "")
                          <|> do char '-'
                                 i <- many1 digit
                                 char 'e'
                                 char '-'
                                 j <- many1 digit
                                 return (-(read $ i <> "-e" <> j) ,"") 
                              <|> do i <- many1 digit
                                     char 'e'
                                     char '-'
                                     j <- many1 digit
                                     return (read $ i <> "-e" <> j, "")
                                  <|> do i <- many1 digit
                                         char 'e'
                                         j <- many1 digit
                                         return (read $ i <> "e" <> j, "")
                                      <|> do char '-'
                                             i <- many1 digit
                                             char '.'
                                             j <- many1 digit
                                             return (-(read $ i <> "." <> j), "")
                                          <|> do i <- many1 digit
                                                 char '.'
                                                 j <- many1 digit
                                                 return (read $ i <> "." <> j, "")
                                              <|> do char '-'
                                                     i <- many1 digit
                                                     return (-(read $ i), "")
                                                  <|> do i <- many1 digit
                                                         return (read $ i, "")
                                                      <|> return (0.0, "Parser Error")

space :: Parser ()
space = do many' (sat isSpace)
           return ()

comment :: Parser ()
comment = do string "--"
             many' (sat (/= '\n'))
             return ()

expr :: Parser Double
expr = do n <- natural
          ns <- many' (do s <- symbol "-" 
                          nn <- natural
                          return (s,nn)
                       <|> do s <- symbol "+"
                              nn <- natural
                              return (s,nn))
          case operation n ns of
               Right err -> failure
               Left q -> return q

operation :: Double -> [(String, Double)] -> Either Double String
operation x [] = Left x
operation x (y:ys) = case fst y of 
                      "-" -> if isInfinite (x - snd y) then Right "Overflow Error"
                             else operation (x - snd y) ys
                      "+" -> if isInfinite (x + snd y) then Right "Overflow Error"
                             else operation (x + snd y) ys
                      "/" -> case snd y of 
                                  0.0 -> Right "Division by Zero"
                                  otherwise -> if isInfinite (x / snd y) then Right "Overflow Error"
                                               else operation (x / snd y) ys
                      "*" -> if isInfinite (x * snd y) then Right "Overflow Error"
                             else operation (x * snd y) ys
                      "Division by Zero" -> Right "Division by Zero"
                      "Parser Error" -> Right "Parser Error"
                      "Use Before Defined" -> Right "Use Before Defined"
                      "Overflow Error" -> Right "Overflow Error"
                 

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Double
natural = token nat

integer :: Parser Double
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

p :: Parser (Char,Char)
p = do x <- item
       item
       y<-item
       return (x,y)

q:: Parser String
q = do char '['
       d  <- digit
       ds <- many (do char ','
                      digit)
       char ']'
       return (d:ds)

replace :: [(String, Double)] -> [(String, Double)]  -> String -> Double -> [(String, Double)] 
replace [] xs y z = xs
replace ((a,b):ws) xs y z = if a == y then [(a,z)] ++ ws ++ xs
                            else replace ws ([(a,b)] ++ xs) y z
line' :: [(String, Double)] -> Parser (Maybe Double, [(String, Double)], String)
line' st = do i <- ident 
              char '='
              e <- expr' st
              case snd e of
                   "" -> case lookup i st of
                              Nothing -> return (Nothing, st ++ [(i,fst e)], "")
                              otherwise -> return (Nothing, replace st [] i (fst e), "")
                   otherwise -> return (Nothing, st, (snd e))
           <|> do e <- expr' st
                  case snd e of
                       "" -> return ((Just (fst e)), st, "")
                       otherwise -> return (Nothing, st, (snd e))

expr':: [(String, Double)] -> Parser (Double, String)
expr' st = do t <- term' st
              case snd t of
                   "" -> do es <- many' (do s <- symbol "-"
                                            e <- term' st
                                            case snd e of
                                                 "" -> return (s, fst e)
                                                 otherwise -> return ((snd e), 0.0)
                                         <|> do s <- symbol "+"
                                                e <- term' st
                                                case snd e of
                                                     "" -> return (s, fst e)
                                                     otherwise -> return ((snd e), 0.0))
                            case operation (fst t) es of
                                 Right err -> return (0.0, err)
                                 Left q -> return (q, "")
                   otherwise -> return (0.0, snd t)
           <|> do t <- term' st
                  case snd t of
                       "" -> return ((fst t), "")
                       otherwise -> return (0.0, snd t)

term':: [(String, Double)] -> Parser (Double, String)
term' st = do f <- factor' st
              case snd f of 
                   "" -> do es <- many' (do s <- symbol "/"
                                            e <- factor' st                                          
                                            case snd e of 
                                                 "" -> case fst e of 
                                                            0.0 -> return ("Division by Zero", 0.0)
                                                            otherwise -> return (s, fst e)
                                                 otherwise -> return ((snd e), 0.0)
                                         <|> do s <- symbol "*"
                                                e <- factor' st
                                                case snd e of
                                                     "" -> return (s, fst e)
                                                     otherwise -> return ((snd e), 0.0))
                            case operation (fst f) es of
                                 Right err -> return (0.0, err)
                                 Left q -> return (q, "")
                   otherwise -> return (0.0, snd f)
            <|> do f <- factor' st
                   case snd f of
                        "" -> return ((fst f), "")
                        otherwise -> return (0.0, snd f)

factor':: [(String, Double)] -> Parser (Double, String)
factor' st =  do s <- statement' st
                 case snd s of 
                      "" -> do char '^'
                               f <- factor' st
                               case snd f of
                                    "" -> return (((fst s)**(fst f)), "")
                                    otherwise -> return (0.0, snd f)
                      otherwise -> return (0.0, snd s)
             <|> do s <- statement' st
                    case snd s of
                         "" -> return ((fst s), "")
                         otherwise -> return (0.0, snd s)
                          
statement' :: [(String, Double)] -> Parser (Double, String)
statement' st = do char '('
                   e <- expr' st
                   case snd e of
                        "" -> do char ')'
                                 return (fst e, "")
                        otherwise -> return (0.0, snd e)
                <|> do char '-'
                       char '('
                       e <- expr' st
                       case snd e of
                            "" -> do char ')'
                                     return ((-1.0 * fst e), "")
                            otherwise -> return (0.0, snd e)
                    <|> do i <- ident
                           case getVal st i of 
                                Left num -> return (num , "")
                                Right err -> return (0.0, err)
                        <|> do char '-'
                               i <- ident
                               case getVal st i of 
                                    Left num -> return ((-1.0 * num) , "")
                                    Right err -> return (0.0, err)
                            <|> do d <- double
                                   case snd d of
                                        "" -> if isInfinite (fst d) then return (0.0, "Overflow Error")
                                              else return (fst d, "")
                                        otherwise -> return (0.0, snd d)
                                <|> return (0.0, "Parser Error")

getVal :: [(String, Double)] -> String -> Either Double String
getVal [] y = Right "Use Before Defined"
getVal (x:xs) y = if fst x == y then Left (snd x)
                  else getVal xs y

fst' (a,_,_) = a

snd' (_,a,_) = a

thr' (_,_,a) = a

eval' :: [String] -> [Double] -> [(String, Double)] -> Either [Double] String
eval' [] ys st = Left ys
eval' (x:xs) ys st = case thr' (fst (head (parse (line' st) x))) of 
                          "" -> case fst' (fst (head (parse (line' st) x))) of
                                        Nothing -> eval' xs ys (snd' (fst (head (parse (line' st) x))))
                                        Just t' -> eval' xs (ys ++ [t']) (snd' (fst (head (parse (line' st) x))))
                          otherwise -> Right (thr' (fst (head (parse (line' st) x))))

eval :: String -> Either [Double] String
eval xs =  case eval' (filterComment (wordsWhen (==';') (filter (/=' ') xs))) [] [] of
                Left a -> Left a
                Right b -> Right b

putStr' :: String -> IO ()
putStr' s =  sequence_ (map putChar s)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

filterComment :: [String] -> [String]
filterComment xs = filter (not . isComment) xs

isComment :: String -> Bool
isComment (a:b:_) = a == '-' && b == '-'
isComment _ = False

-- AST things

asteval' :: [String] -> [Expr] -> Either [Expr] String
asteval' [] ys = Left ys
asteval' (x:xs) ys = case snd (fst (head (parse astline' x))) of 
                          "" ->  asteval' xs ([(fst (fst (head (parse astline' x))))] ++ ys)
                          otherwise -> Right (snd (fst (head (parse astline' x))))
                        

asteval :: String -> Either [Expr] String
asteval xs = case asteval'  (filterComment (wordsWhen (==';') (filter (/=' ') xs))) [] of
                  Left a -> Left a
                  Right b -> Right b

produce_Ast :: String -> Either AST String
produce_Ast xs = case asteval xs of
                      Left a -> Left (AST (reverse (a)))
                      Right b -> Right b 

produce_ast' :: [Expr] -> [String] -> [String]
produce_ast' [] ys = ys
produce_ast' (x:xs) ys = produce_ast' xs ([buildTreeString x] ++ ys)

data AST = AST [Expr]

instance Show AST where
         show (AST list) = show list

getListExpr :: AST -> [Expr]
getListExpr (AST list) = list

data Expr = Literal Double 
          | Variable Var 
          | Assign Var Expr 
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr                      
          | Div Expr Expr 
          | Exp Expr Expr

isLiteral :: Expr -> Bool
isLiteral (Literal a) = True
isLiteral _ = False

getLiteral :: Expr -> Double
getLiteral (Literal a) = a

isVariable :: Expr -> Bool
isVariable (Variable a) = True
isVariable _ = False

getVariable :: Expr -> String
getVariable (Variable a) = getVar a

isAssign:: Expr -> Bool
isAssign (Assign a b) = True
isAssign _ = False

getVarAssign :: Expr -> String
getVarAssign (Assign a b) = getVar a

getExprAssign :: Expr -> Expr
getExprAssign (Assign a b) = b

isAdd :: Expr -> Bool
isAdd (Add a b) = True
isAdd _ = False

getAddAExpr :: Expr -> Expr
getAddAExpr (Add a b) = a

getAddBExpr :: Expr -> Expr
getAddBExpr (Add a b) = b

isSub :: Expr -> Bool
isSub (Sub a b) = True
isSub _ = False

getSubAExpr :: Expr -> Expr
getSubAExpr (Sub a b) = a

getSubBExpr :: Expr -> Expr
getSubBExpr (Sub a b) = b

isMul :: Expr -> Bool
isMul (Mul a b) = True
isMul _ = False

getMulAExpr :: Expr -> Expr
getMulAExpr (Mul a b) = a

getMulBExpr :: Expr -> Expr
getMulBExpr (Mul a b) = b

isDiv :: Expr -> Bool
isDiv (Div a b) = True
isDiv _ = False

getDivAExpr :: Expr -> Expr
getDivAExpr (Div a b) = a

getDivBExpr :: Expr -> Expr
getDivBExpr (Div a b) = b

isExp :: Expr -> Bool
isExp (Exp a b) = True
isExp _ = False

getExpAExpr :: Expr -> Expr
getExpAExpr (Exp a b) = a

getExpBExpr :: Expr -> Expr
getExpBExpr (Exp a b) = b

instance Show Expr where
         show (Literal d)  = show d
         show (Variable v) = show v
         show (Assign v e) = "(" ++ show v ++ " = " ++ show e ++ ")"
         show (Add a b)    = "(" ++ show a ++ " + " ++ show b ++ ")"
         show (Sub a b)    = "(" ++ show a ++ " - " ++ show b ++ ")"
         show (Mul a b)    = "(" ++ show a ++ " * " ++ show b ++ ")"
         show (Div a b)    = "(" ++ show a ++ " / " ++ show b ++ ")"
         show (Exp a b)    = "(" ++ show a ++ " ^ " ++ show b ++ ")"
         
newtype Var = Var String deriving (Ord, Eq, Show)

getVar :: Var -> String
getVar (Var a) = a

operation' :: Expr -> [(String, Expr)] -> Either Expr String
operation' x [] = Left x
operation' x (y:ys) = case fst y of 
                           "-" -> operation' (Sub x (snd y)) ys
                           "+" -> operation' (Add x (snd y)) ys
                           "/" -> operation' (Div x (snd y)) ys
                           "*" -> operation' (Mul x (snd y)) ys
                           "Parser Error" -> Right "Parser Error"

chainleft :: Parser a -> Parser (a -> a -> a) -> Parser a
chainleft p op = do {a <- p; rest a}
                     where
                            rest a = (do f <- op
                                         b <- p
                                         rest (f a b))
                                      +++ return a

addop = do {symbol "+"; return (+)} +++ do {symbol "-"; return (-)}
mulop = do {symbol "*"; return (*)} +++ do {symbol "/"; return (/)} 
expop = do {symbol "^"; return (**)}



astline' :: Parser (Expr, String)
astline' = do i <- ident 
              char '='
              e <- astexpr'
              case snd e of
                   "" -> return ((Assign (Var i) (fst e)), "")
                   otherwise -> return ((Literal (0.0)), snd e)
           <|> astexpr'

astexpr':: Parser (Expr, String)
astexpr' = do t <- astterm'
              case snd t of
                   "" -> do es <- many' (do s <- symbol "-"
                                            e <- astterm'
                                            case snd e of
                                                 "" -> return (s, fst e)
                                                 otherwise -> return ("Parser Error", (Literal (0.0)))
                                         <|> do s <- symbol "+"
                                                e <- astterm'
                                                case snd e of
                                                     "" -> return (s, fst e)
                                                     otherwise -> return ("Parser Error", (Literal (0.0))))
                            case (operation' (fst t) es) of
                                 Left a -> return (a, "")
                                 Right b -> return ((Literal (0.0)), b)
                   otherwise -> return ((Literal (0.0)), snd t)
           <|> astterm'

astterm':: Parser (Expr, String)
astterm' = do f <- astfactor'
              case snd f of
                   "" -> do es <- many' (do s <- symbol "/"
                                            e <- astfactor'
                                            case snd e of
                                                 "" -> return (s, fst e)
                                                 otherwise -> return ("Parser Error", (Literal (0.0)))
                                         <|> do s <- symbol "*"
                                                e <- astfactor'
                                                case snd e of 
                                                     "" -> return (s, fst e)
                                                     otherwise -> return ("Parser Error", (Literal (0.0))))
                            case (operation' (fst f) es) of 
                                 Left a -> return (a, "")
                                 Right b -> return ((Literal (0.0)), b)
                   otherwise -> return ((Literal (0.0)), snd f)
           <|> astfactor'

astfactor':: Parser (Expr, String)
astfactor' =  do s <- aststatement'
                 case snd s of
                      "" -> do c <- symbol "^"
                               f <- astfactor'
                               case snd f of 
                                    "" -> return ((Exp (fst s) (fst f)), "")
                                    otherwise -> return (Literal (0.0), snd f)
                      otherwise -> return (Literal (0.0), snd s)
              <|> aststatement'
                          
aststatement' :: Parser (Expr, String)
aststatement' = do o <- symbol "("
                   e <- astexpr'
                   case snd e of
                        "" -> do c <- symbol ")"
                                 return (fst e, "")
                        otherwise -> return (Literal (0.0), snd e)
                <|> do char '-'
                       o <- symbol "("
                       e <- astexpr'
                       case snd e of
                            "" -> do c <- symbol ")"
                                     return ((Mul (Literal (-1.0)) (fst e)), "")
                            otherwise -> return (Literal (0.0), snd e)
                <|> do i <- ident
                       return ((Variable (Var i)), "")
                    <|> do char '-'
                           i <- ident
                           return ((Mul (Literal (-1.0)) (Variable (Var (i)))), "")
                        <|> do d <- double
                               case snd d of
                                    "" -> return ((Literal (fst d)), "")
                                    otherwise -> return ((Literal (0.0)), snd d)
                            <|> return ((Literal (0.0)), "Parser Error")

buildTreeVar :: Var -> Btree String
buildTreeVar v = Node (show v) Empty Empty

buildTree :: Expr -> Btree String
buildTree ex = case ex of
                    Literal d  -> Node (show d) Empty Empty
                    Variable v -> Node (show v) Empty Empty
                    Assign v e -> Node "=" (buildTreeVar v) (buildTree e)
                    Add a b    -> Node "+" (buildTree a) (buildTree b)
                    Sub a b    -> Node "-" (buildTree a) (buildTree b)
                    Mul a b    -> Node "*" (buildTree a) (buildTree b)
                    Div a b    -> Node "/" (buildTree a) (buildTree b)
                    Exp a b    -> Node "^" (buildTree a) (buildTree b)


buildTreeVarString :: Var -> String
buildTreeVarString v = "(Node " ++ (show v) ++ " Empty " ++ "Empty)"

buildTreeString :: Expr -> String
buildTreeString ex = case ex of
                    Literal d  -> "(Node " ++ (show d) ++ " Empty " ++ "Empty)"
                    Variable v -> "(Node " ++ (show v) ++ " Empty " ++ "Empty)"
                    Assign v e -> "(Node " ++ ['"','=','"'] ++ (buildTreeVarString v) ++ (buildTreeString e)
                    Add a b    -> "(Node " ++ ['"','+','"'] ++ (buildTreeString a) ++ (buildTreeString b)
                    Sub a b    -> "(Node " ++ ['"','-','"'] ++ (buildTreeString a) ++ (buildTreeString b)
                    Mul a b    -> "(Node " ++ ['"','*','"'] ++ (buildTreeString a) ++ (buildTreeString b)
                    Div a b    -> "(Node " ++ ['"','/','"'] ++ (buildTreeString a) ++ (buildTreeString b)
                    Exp a b    -> "(Node " ++ ['"','^','"'] ++ (buildTreeString a) ++ (buildTreeString b)

eval_Ast :: Either AST String -> Either [Double] String
eval_Ast ast = case ast of 
                    Left a -> case eval_Ast' (getListExpr a) [] [] of 
                                   Left d -> Left d
                                   Right err -> Right err
                    Right b -> Right b

eval_Ast' :: [Expr] -> [Double] -> [(String, Double)] -> Either [Double] String
eval_Ast' [] ys st = Left ys
eval_Ast' (x:xs) ys st = case evaluate' x st of 
                              Left e -> case fst e of 
                                             Nothing -> eval_Ast' xs ys (snd e)
                                             Just t' -> eval_Ast' xs (ys ++ [t']) (snd e)
                              Right err -> Right (fst err)

evaluate' :: Expr -> [(String, Double)] -> Either (Maybe Double, [(String, Double)]) (String, [(String, Double)])
evaluate' x st = if isLiteral x then Left (Just (getLiteral x), st)
                 else if isVariable x then case getVal st (getVariable x) of 
                                                Left num -> Left (Just num, st)
                                                Right err -> Right (err, st) 
                 else if isAssign x then  case lookup (getVarAssign x) st of
                                               Nothing -> case evaluate' (getExprAssign x) st of 
                                                               Left a -> case fst a of
                                                                              Nothing -> Left (Nothing, snd a)
                                                                              Just a' -> Left (Nothing, (snd a) ++ [(getVarAssign x, a')])
                                                               Right err -> Right (fst err, st)
                                               otherwise -> case evaluate' (getExprAssign x) st of
                                                                 Left a -> case fst a of
                                                                                Nothing -> Left (Nothing, snd a)
                                                                                Just a' -> Left (Nothing, replace st [] (getVarAssign x) a')
                                                                 Right err -> Right (fst err, st)
                 else if isAdd x then case evaluate' (getAddAExpr x) st of 
                                           Left a -> case fst a of 
                                                          Nothing -> Left (Nothing, snd a)
                                                          Just a' -> case evaluate' (getAddBExpr x) (snd a) of
                                                                          Left b -> case fst b of 
                                                                                         Nothing -> Left (Nothing, snd b)
                                                                                         Just b' -> Left (Just (a' + b'), snd b)
                                                                          Right err -> Right (fst err, st)
                                           Right err -> Right (fst err, st)
                 else if isSub x then case evaluate' (getSubAExpr x) st of 
                                           Left a -> case fst a of 
                                                          Nothing -> Left (Nothing, snd a)
                                                          Just a' -> case evaluate' (getSubBExpr x) (snd a) of
                                                                          Left b -> case fst b of 
                                                                                         Nothing -> Left (Nothing, snd b)
                                                                                         Just b' -> Left (Just (a' - b'), snd b)
                                                                          Right err -> Right (fst err, st)
                                           Right err -> Right (fst err, st)
                 else if isMul x then case evaluate' (getMulAExpr x) st of 
                                           Left a -> case fst a of 
                                                          Nothing -> Left (Nothing, snd a)
                                                          Just a' -> case evaluate' (getMulBExpr x) (snd a) of
                                                                          Left b -> case fst b of 
                                                                                         Nothing -> Left (Nothing, snd b)
                                                                                         Just b' -> Left (Just (a' * b'), snd b)
                                                                          Right err -> Right (fst err, st)
                                           Right err -> Right (fst err, st)
                 else if isDiv x then case evaluate' (getDivAExpr x) st of 
                                           Left a -> case fst a of 
                                                          Nothing -> Left (Nothing, snd a)
                                                          Just a' -> case evaluate' (getDivBExpr x) (snd a) of
                                                                          Left b -> case fst b of 
                                                                                         Nothing -> Left (Nothing, snd b)
                                                                                         Just b' -> case b' of 
                                                                                                         0.0 -> Right ("Division by Zero", st)
                                                                                                         otherwise -> Left (Just (a' / b'), snd b)
                                                                          Right err -> Right (fst err, st)
                                           Right err -> Right (fst err, st)
                 else if isExp x then case evaluate' (getExpAExpr x) st of 
                                           Left a -> case fst a of 
                                                          Nothing -> Left (Nothing, snd a)
                                                          Just a' -> case evaluate' (getExpBExpr x) (snd a) of
                                                                          Left b -> case fst b of 
                                                                                         Nothing -> Left (Nothing, snd b)
                                                                                         Just b' -> Left (Just (a' ** b'), snd b)
                                                                          Right err -> Right (fst err, st)
                                           Right err -> Right (fst err, st)
                 else Right ("Parsing Error", st)

-- Btree implementation 
data Btree a = Empty | Node a (Btree a) (Btree a) deriving Show
tr_l = Node "b" (Node "d" Empty Empty) (Node "e" Empty Empty)
tr_r = Node "c" (Node "f" Empty Empty) (Node "g" Empty Empty)
tr = Node "a" tr_l tr_r :: Btree String
tx = (Node "x" tr tr)

trr = Node "a" (Node "b" (Node "c" (Node "d" Empty (Node "f" Empty tx)) Empty) Empty) Empty:: Btree String

data ParentDir = PLeft | PRight | NoParent deriving (Show,Eq)
type ParentPos = Int
type Level = Int

dline = '|'
factor = 4

m c1 c2 = if c1 == dline then c1 else c2
zipWith' f xs [] = xs
zipWith' f [] xs = xs
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

build_line pd a pp level = foldl (zipWith' m) "" (((++"|").(flip replicate ' ') <$> (factor*) <$> pp)++[(replicate (factor*level) ' ')++cn++show a])
                           where cn = case pd of PLeft -> "└──"
                                                 PRight -> "┌──"
                                                 NoParent -> "───"

tprint :: Show a => ParentDir -> [ParentPos] -> Level -> Btree a -> [String]
tprint _ _ _ Empty = []
tprint pd pp level (Node a l r) = tprint PRight new_pp_r (level+1) r ++
                                  [build_line pd a pp level] ++
                                  tprint PLeft new_pp_l (level+1) l
                                  where new_pp_r = case pd of PRight -> pp
                                                              PLeft -> pp++[level]
                                                              NoParent -> pp
                                        new_pp_l = case pd of PRight -> pp++[level]
                                                              PLeft -> pp
                                                              NoParent -> pp

printt t = putStr $ (intercalate "\r\n" (tprint NoParent [] 0 t))++"\r\n"

-- End Btree