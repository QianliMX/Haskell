module TinyParser where


import           Control.Monad
import           Control.Applicative
import           System.IO 

import qualified Data.ByteString.Lazy as B  
import qualified Data.ByteString as S  
import qualified Data.ByteString.Char8 as C








data Exp = IntExp Int
         | Var String
         | Plus Exp Exp
         | Times Exp Exp deriving (Read, Eq)
         
data Stmt = Skip
          | Assign String Exp
          | Seq Stmt Stmt       -- Stmt; Stmt
          | If Exp Stmt Stmt 
          | While Exp Stmt 

          | PushHead
          | PopHead String
          | PushVar String 
          | PopVar  String deriving (Read, Eq)
           
instance Show Exp where 
  show (IntExp x) = show x 
  show (Var str) = str
  show (Plus e1 e2) = show e1 ++ " + " ++ show e2
  show (Times e1 e2) = show e1 ++ " * " ++ show e2
  
instance Show Stmt where 
  show (Skip) = "skip" ++ ";\n"
  show (Assign str e) = str ++ " := " ++ show e ++ ";\n"
  show (Seq s1 s2) = show s1 ++ show s2
  show (If e s1 s2) = "if " ++ show e ++ ";\n" ++ 
                      show s1 ++ "\n" ++ show s2
  show (While e s) = "while " ++ show e ++ ";\n" ++ show s
  
  show (PushHead) = "pushhead" ++ ";\n"
  show (PopHead str) = "pophead " ++ str ++ ";\n"
  show (PushVar str) = "pushvar " ++ str ++ ";\n"
  show (PopVar  str) = "popvar " ++ str ++ ";\n"

            






-- Idea from the ImpParser chapter of "Software foundation"
-- the reason I can't use "readFile" is for that, this software is designed in a sandbox 

-- (1) Lexical Analysis

isWhite :: Char -> Bool
isWhite c = 
  or [(beq_char_b ' '),
      (beq_char_b '\t'),
      (beq_char_b '\n'),
      (beq_char_b '\r')]
  where beq_char_b = (\ c' -> c == c')
  
isAlpha :: Char -> Bool
isAlpha c = 
  or [ (and [('A' <= c), (c <= 'Z')]),
       (and [('a' <= c), (c <= 'z')])]
       
isDigit :: Char -> Bool
isDigit c = and [('0' <= c), (c <= '9')]

data CharType = White | Alpha | Digit | Other deriving (Show, Read, Eq)
classifyChar :: Char -> CharType
classifyChar c = if (isWhite c) then White
            else if (isAlpha c) then Alpha
            else if (isDigit c) then Digit
            else Other
    
-- why need to reverse the tk here?        
type Token = String
tokenize_helper :: CharType -> String -> String -> [String]
tokenize_helper cls acc xs = 
  let tk = case acc of []   -> []
                       _:_  -> [reverse acc] in 
  case xs of 
    []    -> tk
    x:xs' -> compare cls (classifyChar x) x where
      compare _ _ '(' = tk ++ ['('] : (tokenize_helper Other [] xs')
      compare _ _ ')' = tk ++ [')'] : (tokenize_helper Other [] xs')
      compare _ White _ = tk ++ (tokenize_helper White [] xs')
      compare Alpha Alpha x = (tokenize_helper Alpha (x:acc) xs')
      compare Digit Digit x = (tokenize_helper Digit (x:acc) xs')
      compare Other Other x = (tokenize_helper Other (x:acc) xs')
      compare _ tp x = tk ++ (tokenize_helper tp [x] xs')
      
tokenize :: String -> [String]
tokenize s = tokenize_helper White [] s
      






-- (2) Adding tags to the list of tokens

data Tag = T_Skip | T_Num | T_Assign | T_Var | T_Semicolon | T_While | T_If | T_LBranket | T_RBranket | T_Plus | T_Mult | T_Minus | T_Error deriving (Show, Eq)

t_Num :: String -> String -> (String, Tag)
t_Num []   acc = (reverse acc, T_Num)
t_Num (x:xs) acc = if (isDigit x) then t_Num xs (x:acc)
                   else ((reverse acc) ++ (x:xs)  , T_Error)

-- what is the priority of ":" here?                               
t_Var :: String -> String -> (String, Tag)
t_Var []   acc = case (reverse acc) of
                      "while"   -> ("while", T_While)
                      "if"      -> ("if"   , T_If)
                      "skip"    -> ("skip" , T_Skip)
                      xs        -> (xs,      T_Var)                   
t_Var (x:xs) acc = if (isAlpha x) then t_Var xs (x:acc)
                   else ((reverse acc) ++ (x:xs)  , T_Error)

tag_helper :: String -> (String, Tag)
tag_helper []  = ("", T_Error)
tag_helper [x] =    if (x == ';')  then (";", T_Semicolon) 
               else if (x == '(')  then ("(", T_LBranket)
               else if (x == ')')  then (")", T_RBranket)
               else if (x == '+')  then ("+", T_Plus)
               else if (x == '*')  then ("*", T_Mult)
               else if (x == '-')  then ("-", T_Minus)
               else if (isDigit x) then ([x], T_Num)
               else if (isAlpha x) then ([x], T_Var)
               else ([x], T_Error)
tag_helper (x:xs) = if (isDigit x) then (t_Num xs [x])
               else if (isAlpha x) then (t_Var xs [x])
               else if (x == ':')  then case xs of
                                        [x']     -> if (x' == '=')
                                                    then (":=", T_Assign)
                                                    else (x:xs , T_Error)
                                        x':xs'   -> (x:xs, T_Error)
               else (x:xs, T_Error) 
                           
tag :: [String] -> [(String, Tag)] 
tag [x] = [tag_helper (x)]
tag (x:xs) = (tag_helper (x)) : (tag xs)
tag []  = [("", T_Error)]









-- (3) Monadic Recursive Descent Parser
  
newtype Parser a = Parser ([(String, Tag)] -> [(a, [(String, Tag)])])

parse :: Parser a -> ([(String, Tag)] -> [(a, [(String, Tag)])]) 
parse (Parser f) = f 

instance Functor Parser where
    fmap = liftM 
    
instance Applicative Parser where
    pure a = Parser (\inp -> [(a, inp)]) -- return a
    (<*>) = ap 
    
instance Monad Parser where
    (Parser p) >>= f =                  -- `bind`
        Parser (\inp -> concat [parse (f v) out | (v,out) <- p inp])
    
instance Alternative Parser where
    empty = Parser (\inp -> [])
    p <|> q = Parser (\inp -> parse p inp ++ parse q inp)
              

{-
Exp  -> Term Exp'
Exp' -> + Term Exp'               : Exp -> Exp
      | Epsilon                  
Term  -> Fac Term'                : Exp
Term' -> * Fac Term'              : Exp -> Exp
       | Epsilon 
Fac  -> (Exp)                     : Exp
      | Num
      | String(name) -}

item :: Parser (String, Tag)
item = Parser (\inp -> case inp of 
  []     -> []
  (x:xs) -> [(x , xs)])
  
sat :: Tag -> (Parser String)
sat t' = do {
           (s, t) <- item   ;
           guard ( t == t') ;
           pure s }
           
num :: Parser Exp
num = do {
         s <- (sat T_Num)   ;
         pure (IntExp (read s)) } <|>
      do {
         sat T_Minus  ;
         s  <- (sat T_Num)   ;
         pure (IntExp (read ( "-" ++ s )))}
         
var :: Parser Exp
var = do {
          v  <- sat T_Var   ;
          pure (Var v) }
         
fac :: Parser Exp
fac = num <|> var
          <|> 
      do { 
         (sat T_LBranket) ;
         e <-  expr       ;
         (sat T_RBranket) ;
         pure (e)          }         

term' :: Parser (Exp -> Exp)
term' = do {
           s        <- (sat T_Mult)   ;
           e1       <- fac            ;
           f        <- term'          ;
           pure (\e -> f (Times e e1)) }    
        <|>
        pure (\p -> p ) -- the form of epsilon
           
term :: Parser Exp
term = do {
          e1        <- fac             ;
          f         <- term'           ;
          pure ( f  e1)                 }  
          
expr' :: Parser (Exp -> Exp)
expr' = do {
           s        <- (sat T_Plus)   ;
           e1       <- term           ;
           f        <- expr'          ;
           pure (\e -> f (Plus e e1))  }    
        <|>
        pure (\p -> p )
        
expr :: Parser Exp
expr = do {
          e1       <- term             ;
          f        <- expr'            ;
          pure ( f  e1 )                 }   <|>
       do {
          sat T_LBranket;
          e <- expr;
          sat T_RBranket;
          pure (e)}
          
        
{-BNF
stmt -> skip <|>
        stmt ; stmt <|>
        string := exp <|>
        if exp stmt stmt <|>
        while exp stmt-}
        
{-
stmt  -> block stmt' 
stmt' -> ; block stmt' |
         epsilo
block -> skip |
         string := exp    |
         if exp stmt stmt |
         while exp stmt |
         (stmt) -}
s_skip :: Parser Stmt
s_skip = do {
          sat T_Skip;
          pure (Skip)}   

s_if :: Parser Stmt
s_if = do {
           (sat T_If);
           e <- expr;
           s1 <- stmt;
           s2 <- stmt;
           pure (If e s1 s2)}
           
s_assign :: Parser Stmt
s_assign = do {
           v <- (sat T_Var);
           (sat T_Assign);
           e <- expr;
           pure (Assign v e)}
           
s_while :: Parser Stmt
s_while = do {
           (sat T_While);
           e <- expr;
           s <- stmt;
           pure (While e s)}
      
block :: Parser Stmt
block = s_skip    <|> 
        s_assign  <|>
        s_if      <|>
        s_while   <|>
        do {
           sat T_LBranket;
           b <- stmt;
           sat T_RBranket;
           pure (b)}
           
stmt' :: Parser (Stmt -> Stmt)
stmt' = do {
           (sat T_Semicolon);
           b'  <- block ;  
           f   <- stmt' ;
           pure (\b -> (Seq b (f b')))} <|>
        do { 
           pure (\b -> b) }
           
stmt :: Parser Stmt
stmt =  do {
          b <- block;
          f <- stmt';
          pure(f b)}
          
goal :: Parser Stmt
goal = stmt



{-

handle <- openFile "code1.txt" ReadMode

contents <- hGetContents handle

let a = fst $ head $ (parse goal) $ tag $ tokenize contents 

a

--putStr $ show a 


-}






























