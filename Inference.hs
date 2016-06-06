import Data.Char
import System.Exit

data Tk = TkVar String
		| TkLp
		| TkRp
		| TkLb
		| TkRb
		| TkBtick
		| TkPtype String
		| TkRt
		| TkAmp
		| TkComma
		| N
		deriving (Eq, Show)
		
tokenize :: String -> [Tk]
tokenize [] = []
tokenize (x:xs) 
		| x == '(' = TkLp : tokenize xs
		| x == ')' = TkRp : tokenize xs
		| x == '[' = TkLb : tokenize xs
		| x == ']' = TkRb : tokenize xs
		| x == '-' = arrow x xs
		| x == ' ' = tokenize xs
		| x == ',' = TkComma : tokenize xs
		-- x `elem` ['a'..'z'] || x `elem` ['A'..'Z'] = name x xs
		| x == '`' = name x xs
		| x == 'i' || x == 'r' || x == 's' = intFunc x xs
		| otherwise = N:[]

intFunc :: Char -> String -> [Tk]
intFunc x xs = let (namestr, ys) = span isAlpha xs in 
					if((x:namestr) == "int" || (x:namestr) == "real" || (x:namestr) == "str")
					then TkPtype (x:namestr) : tokenize ys
					else N:[]
								
arrow :: Char -> String -> [Tk]
arrow x (y:ys)
		| y == '>' = TkRt : tokenize ys
		| otherwise = N:[]

name :: Char -> String -> [Tk]
name x (y:ys) 
		| isAlpha y = var x (y:ys)
		| otherwise = N:[]
			
var :: Char -> String -> [Tk]
var x (y:ys) = let (namestr, zs) = span isAlphaNum ys in
				TkVar (x:y:namestr) : tokenize zs


-- Checking Grammar
			
parse :: [Tk] -> [Tk]
parse tokens = let retokens = typeFunc tokens
				in retokens
					
					
lookAhead :: [Tk] -> Tk
lookAhead [] = N
lookAhead (x:xs) = x

next :: [Tk] -> [Tk]
next (x:xs) = xs

typeFunc :: [Tk] -> [Tk]
typeFunc tokens = 
			case lookAhead tokens of 
				(TkPtype str) -> primFunc tokens									
				TkLp          -> funcFunc tokens									
				(TkVar str)      -> typeVarFunc tokens									
				TkLb          -> listFunc tokens								
				_  -> [N]

funcFunc :: [Tk] -> [Tk]
funcFunc tokens = 
			case lookAhead tokens of
				TkLp -> if(lookAhead tokens' == TkRp)
						then funcFunc tokens'					
						else argFunc tokens'							
				TkRt -> typeFunc tokens'							
				TkRp -> funcFunc tokens'
				_ -> [N]						
			where tokens' = next tokens
										
										
argFunc :: [Tk] -> [Tk]
argFunc tokens = 
			let tokens' = typeFunc tokens
			in
				case lookAhead tokens' of
					TkComma -> argFunc (next tokens')										
					TkRp -> funcFunc (next tokens')								
					_ -> [N]
			  
listFunc :: [Tk] -> [Tk]
listFunc (x:tokens) =
			let tokens' = typeFunc tokens
				in
					case lookAhead tokens' of
						TkRb -> next tokens'
						_ -> [N]			
													  
typeVarFunc :: [Tk] -> [Tk]
typeVarFunc tokens = next tokens
			
			

			
primFunc :: [Tk] -> [Tk]
primFunc tokens 
		| lookAhead tokens == TkPtype "int" = next tokens
		| lookAhead tokens == TkPtype "real" = next tokens
		| lookAhead tokens == TkPtype "str" = next tokens
		| otherwise = [N]
		
	
--Unification

unify :: [Tk] -> [Tk] -> [Tk]
unify [] [] = []
unify (x:xs) [] = if x == TkRb 
					then TkRb : unify xs []
					else N:[] 
unify [] (x:xs) = if x == TkRb 
					then TkRb : unify [] xs
					else N:[] 
unify (x:xs) (y:ys) = 
			case (x,y) of 
				(TkLb, TkPtype str) -> N:[]
				(TkPtype str, TkLb) -> N:[]
				(TkPtype str, TkVar str1) -> TkPtype str: unify xs ys
				(TkVar str1, TkPtype str) -> TkPtype str: unify xs ys
				(TkPtype str1, TkPtype str2) -> if(str1 == str2)
													then TkPtype str1:unify xs ys
													else N:[]
				(TkVar str1, TkVar str2) -> TkVar str1: unify xs ys
				(TkLb, _) -> TkLb : unify xs (y:ys)
				(_, TkLb) -> TkLb : unify (x:xs) ys
				(TkRb, _) -> TkRb : unify xs (y:ys)
				(_, TkRb) -> TkRb : unify (x:xs) ys
				(_, _) -> if (x == y)
							then x: unify xs ys
							else N:[]
				

			
--tktostr
tostr :: [Tk] -> String			
tostr [] = []
tostr (x:xs) = 
			case x of
				TkVar str -> str ++ " " ++ tostr xs
				TkLp -> "( " ++	tostr xs
				TkRp -> ") " ++ tostr xs
				TkLb -> "[ " ++ tostr xs
				TkRb -> "] " ++ tostr xs
				TkPtype str -> str ++ " " ++ tostr xs
				TkRt -> "-> " ++ tostr xs
				TkComma -> ", " ++ tostr xs		
												
									
errchk :: [Tk] -> IO()
errchk tokens
		| N `elem` tokens = do
							putStrLn "ERR"
							exitSuccess
		| otherwise = return()
		
boterrchk :: [Tk] -> IO()
boterrchk tokens
		| N `elem` tokens = do
							putStrLn "BOTTOM"
							exitSuccess
		| otherwise = return()
quitchk :: String -> IO()
quitchk str 
	| str == "QUIT" = exitSuccess
	| otherwise = return()

ampchk :: String -> IO()
ampchk str = if('&' `elem` str)
				then return()
				else do
					putStrLn "ERR"
					exitSuccess
	

loop =	do
		instr <- getLine
		quitchk instr
		ampchk instr
		let (str1, (x:str2)) = break (=='&') instr
		let tokstr1 = tokenize str1
		let tokstr2 = tokenize str2
		errchk tokstr1
		errchk tokstr2	
		let parsing1 = parse tokstr1
		errchk parsing1
		let parsing2 = parse tokstr2
		errchk parsing2
		let unified = unify tokstr1 tokstr2
		boterrchk unified
		let funified = tostr unified
		--print $ tokstr1
		--print $ tokstr2
		--print $ unified
		putStrLn funified; loop
main = loop
	


