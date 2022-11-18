data Formula = Formula{
    leftf :: String,
    op :: String,
    rightf :: String,
    value :: Bool
}

data Node = Node{
    formulas :: [Formula],
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
nextStep ">" True = [False, True, True]
nextStep ">" False= [True, False, False]

--retorna qual indice do operador atual (atualmente com base nos parenteses)
-- expressao indice profundidade 
operator:: String -> Int -> Int -> Int
operator exp index depth
    | index >= length exp && (head exp) == '(' && (last exp) == ')' = operator (removeParentesis exp) 0 0
    | index >= length exp = index-1
    | depth == 0 && ((exp !! index == '&') || (exp !! index == '|')|| (exp !! index == '>') || (exp !! index == '∧') || (exp !! index == '∨') || (exp !! index == '→')) = index
    | exp !! index == '(' = operator exp (index+1) (depth+1)
    | exp !! index == ')' = operator exp (index+1) (depth-1)
    | otherwise = operator exp (index+1) depth

removeParentesis:: String -> String
removeParentesis exp
    | head exp /= '(' = exp
    | otherwise = tail (init exp)

parseExpression:: String -> Int -> [String]
parseExpression exp separator
    | (length exp) == (separator+1) = [exp,"",""]
    | otherwise = [ removeParentesis (take separator exp), [exp !! (separator)], removeParentesis (drop (separator+1) exp)]



formulafy3:: [String] -> Bool -> Formula
formulafy3 exp value = Formula (exp !! 0) (exp !! 1) (exp !! 2) (value)

formulafy2:: String -> Bool -> Formula
formulafy2 exp value = formulafy3 (parseExpression (exp) (operator exp 0 0)) (value)

formulafy1:: Formula -> [Bool] -> [Formula]
formulafy1 form nextstep = [(formulafy2 (leftf form) (nextstep !! 0)), (formulafy2 (rightf form) (nextstep !! 2))]



aplicaRamo:: Node -> [Formula] -> Node
aplicaRamo node forms
    | end node = Node
        (formulas node)
        False
        (Node ([forms !! 0]) True (Node{}) (Node{}))
        (Node ([forms !! 1]) True (Node{}) (Node{}))
    | otherwise = Node
        (formulas node)
        (end node)
        (aplicaRamo (left node) forms)
        (aplicaRamo (right node) forms)
        

branch:: Node -> Int -> Node
branch node index
    | index >= length (formulas node) = Node 
        (formulas node)
        (end node)
        (branch (left node) 0)
        (branch (right node) 0)
    | (op ((formulas node) !! index)) == "" = branch(node)(index+1)
    | nextStep (op ((formulas node) !! index)) (value ((formulas node) !! index)) !! 1 == False = branch (Node 
        (formulas node ++ (formulafy1 ((formulas node) !! index)) (nextStep (op ((formulas node) !! index)) (value ((formulas node) !! index))))
        (end node)
        (left node)
        (right node)
        ) (index + 1)
    | nextStep (op ((formulas node) !! index)) (value ((formulas node) !! index)) !! 1 == True = branch (aplicaRamo 
        (node) 
        (formulafy1 
            ((formulas node) !! index) 
            (nextStep (op ((formulas node) !! index)) (value ((formulas node) !! index)))
        )
    ) (index + 1)
        

concatFormula:: Formula -> String
concatFormula form
    | (op form) == "" = boolToString(value form) ++ ":" ++ leftf form
    | otherwise = boolToString(value form) ++ ":" ++ "(" ++ leftf form ++ ")" ++ op form ++ "(" ++ rightf form ++ ")"
concatFormulas:: [Formula] -> Int -> String
concatFormulas [] depth = ""
concatFormulas form depth = (take (depth*7) (repeat ' ')) ++ concatFormula (form !! 0) ++ "\n" ++ concatFormulas(tail form) depth

showTree:: Node -> Int -> String
showTree node depth
        | end node = concatFormulas (formulas node) (depth)
        | otherwise = (showTree (left node) (depth+1)) ++ concatFormulas (formulas node) (depth) ++ (showTree (right node) (depth+1))


boolToTilda:: Bool -> String
boolToTilda True = ""
boolToTilda False = "~"

gatherAtomic:: [Formula] -> Int -> [Formula]
gatherAtomic forms index
    | index >= length forms = forms
    | (op (forms !! 0)) == "" = gatherAtomic (forms) (index+1)
    | otherwise = gatherAtomic (tail forms) (index)

concatAtomic:: [Formula] -> String
concatAtomic [] = ""
concatAtomic forms = (boolToTilda(value(forms !! 0)))++(leftf(forms !! 0)) ++ "," ++ concatAtomic(tail forms)
concatAtomic forms = (boolToTilda(value(forms !! 0)))++(leftf(forms !! 0)) ++ "," ++ concatAtomic(tail forms)

atomic:: Formula -> Formula
atomic form
    | length (leftf form) == 1 = form
    | otherwise = Formula (tail (leftf form)) ("") ("") (not (value form))

opposite:: Formula -> Formula -> Bool
opposite form1 form2
    | leftf form1 /= leftf form2 = False
    | value form1 == value form2 = False
    | otherwise = True

contradictory:: [Formula] -> Int -> Bool
contradictory [] x = False
contradictory forms x
    | x >= length forms = contradictory (tail forms) 1
    | opposite (atomic(forms !! 0)) (atomic(forms !! x)) == True = True
    | otherwise = contradictory forms (x+1)

validation:: Node -> [Formula] -> String
validation node atomicForms
    | end node && (contradictory (atomicForms ++ (gatherAtomic (formulas node) 0)) (1)) == True = "Valid!"
    | end node = "Counter Model: " ++ concatAtomic(atomicForms ++ (gatherAtomic (formulas node) 0))
    | validation 
        (left node)
        (atomicForms ++ (gatherAtomic (formulas node) 0))
        == "Valid!" = validation (left node) (atomicForms ++ (gatherAtomic (formulas node) 0))
    | validation
        (right node)
        (atomicForms ++ (gatherAtomic (formulas node) 0)) 
        == "Valid!" = validation (right node) (atomicForms ++ (gatherAtomic (formulas node) 0))
    | otherwise = "Counter Model: " ++ concatAtomic(atomicForms ++ (gatherAtomic (formulas node) 0))



main :: IO()
main = do
    -- putStrLn "OBS: Use parenteses entre formulas e operadores (ao menos que sejam atomicos)"
    -- putStrLn "Insira sua formula: "
    -- input <- getLine
    -- let n = read input :: String

    let strteste = "(p|(q&r))>((p|q)&(p|r))"
    let str = "(r&~p)|(~p)"
    let auxstr = strteste
    let parsed = parseExpression (auxstr) (operator (auxstr) 0 0)
    let initial = Node ([Formula (parsed !! 0) (parsed !! 1) (parsed !! 2) (False)]) (True) (Node{}) (Node{})
    let tree = branch (initial) 0
    let valid = validation (tree) ([])
    putStr (valid ++ "\n")
    putStr (showTree tree 0)

