boolToString :: Bool -> String
boolToString True = "V"
boolToString False = "F"

boolToBranch:: Bool -> String
boolToBranch True = ";"
boolToBranch False = "|"

--Return values [before, ifRamify, after]
nextStep:: String -> Bool -> [Bool]
nextStep "∧" True = [True, False, True]
nextStep "∧" False= [False, True, False]
nextStep "∨" True = [True, True, True]
nextStep "∨" False= [False, False, False]
nextStep "→" True = [False, True, True]
nextStep "→" False= [True, False, False]
nextStep "↔" True = []
nextStep "↔" False= []

--retorna qual indice do operador atual (atualmente com base nos parenteses)
-- expressao indice profundidade 
operator:: String -> Int -> Int -> Int
operator exp index depth
    | exp == "" = index-1
    | depth == 0 && ((head exp == '∧') || (head exp == '∨') || (head exp == '→')) = index
    | head exp == '(' = operator (tail exp) (index+1) (depth+1)
    | head exp == ')' = operator (tail exp) (index+1) (depth-1)
    | otherwise = operator (tail exp) (index+1) depth

removeParentesis:: String -> String
removeParentesis exp
    | head exp /= '(' = exp
    | otherwise = tail (init exp)

parseExpression:: String -> Int -> [String]
parseExpression exp separator
    | (length exp) == (separator+1) = [exp]
    | otherwise = [ removeParentesis (take separator exp), [exp !! (separator)], removeParentesis (drop (separator+1) exp)]

branch:: [String] -> Bool -> String
branch exp value
    | length exp == 1 = exp !! 0 ++ ":" ++ boolToString ( head(exp !! 0) == '~')
    | otherwise = boolToString value ++ ":(" ++branch (parseExpression (exp !! 0) (operator (exp !! 0) 0 0)) ((nextStep (exp!!1) value) !! 0) ++ boolToBranch ((nextStep (exp!!1) value)!!1) ++ branch (parseExpression (exp !! 2) (operator (exp !! 2) 0 0)) ((nextStep (exp!!1) value)!! 2) ++ ")"


main = do
    -- putStrLn "frase "
    -- input <- getLine
    -- let n = read input :: StringIO
    -- let n = "(p∨(q∧r))→((p∨q)∧(p∨r))"
    -- let n = "p∨r"
    let str = "(r∧~p)∨(~p)"
    let initial = parseExpression str (operator str 0 0)
    print (branch (initial) False)

--ghc main.hs