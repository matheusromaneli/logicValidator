data Node = Node{
    formula :: String,
    value :: Bool,
    end :: Bool,
    left :: Node,
    right :: Node
}

boolToString :: Bool -> String
boolToString True = "V"
boolToString False = "F"

boolToBranch:: Bool -> String
boolToBranch True = "|"
boolToBranch False = ";"

--Return values [before, ifRamify, after]
nextStep:: String -> Bool -> [Bool]
nextStep "&" True = [True, False, True]
nextStep "&" False= [False, True, False]
nextStep "∧" True = [True, False, True]
nextStep "∧" False= [False, True, False]
nextStep "|" True = [True, True, True]
nextStep "|" False= [False, False, False]
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
    | depth == 0 && ((head exp == '&') || (head exp == '|') || (head exp == '∧') || (head exp == '∨') || (head exp == '→')) = index
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

branch:: [String] -> Bool -> Node
branch exp value
    | length exp == 1 = Node (exp !! 0) (value) True Node{} Node{}
    | otherwise = Node 
        ("(" ++ exp !! 0 ++")"++ exp !! 1 ++ "(" ++ exp !! 2 ++ ")") (value) 
        False
        (branch (parseExpression (exp !! 0) (operator (exp !! 0) 0 0)) ((nextStep (exp!!1) value) !! 0)) 
        (branch (parseExpression (exp !! 2) (operator (exp !! 2) 0 0)) ((nextStep (exp!!1) value)!! 2))

showTree:: Node -> Int -> [String]
showTree node depth
        | end node =[(take (depth*7) (repeat ' ')) ++ boolToString(value node)++":"++(formula node)]
        | otherwise = (showTree (left node) (depth+1))++ [(take depth (repeat ' '))++boolToString(value node)++":"++(formula node)] ++ (showTree (right node) (depth+1))

main :: IO()
main = do
    -- putStrLn "frase "
    -- input <- getLine
    -- let n = read input :: StringIO
    -- let n = "(p∨(q∧r))→((p∨q)∧(p∨r))"
    -- let n = "p∨r"
    let strteste = "(p∨(q∧r))→((p∨q)∧(p∨r))"
    let str = "(r&~p)|(~p)"
    let auxstr = str
    let initial = parseExpression auxstr (operator auxstr 0 0)
    let tree = branch (initial) False
    putStr (unlines (showTree tree 1))



--ghc main.hs