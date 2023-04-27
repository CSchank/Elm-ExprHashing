module HashExpr exposing (..)

import Set
import Dict exposing (Dict)
import Sha256
import Browser
import Html
import Html.Attributes as A
import Html.Events as E
import Parser exposing(Parser, Step, oneOf, succeed, (|=), (|.), end, symbol, spaces, lazy, problem, Step(..), loop)

type alias NodeID = String
type alias ExpressionMap = Dict NodeID HashExpr

type alias RawExpr = (ExpressionMap, NodeID)

type HashExpr = 
      Const Float
    | Var String -- in math we often use Char
    | Sqrt NodeID -- we can take sqrt(anything)
    | IntPow NodeID Int -- the easy case of exponent
    | Exp NodeID -- e^expr
    | Ln NodeID
    | Mult (List NodeID)
    | Add (List NodeID)
    | Neg NodeID
    | Let (String, {- = -} NodeID) {- in -} NodeID

const : Float -> RawExpr
const f = 
    tryHash 0 (Const f) Dict.empty

var : String -> RawExpr
var s =
    tryHash 0 (Var s) Dict.empty

sqrt : RawExpr -> RawExpr
sqrt (exprMap, nodeID) = 
    tryHash 0 (Sqrt nodeID) exprMap

intPow : RawExpr -> Int -> RawExpr
intPow (exprMap, nodeID) i = 
    tryHash 0 (IntPow nodeID i) exprMap

exp : RawExpr -> RawExpr
exp (exprMap, nodeID) = 
    tryHash 0 (Exp nodeID) exprMap

ln : RawExpr -> RawExpr
ln (exprMap, nodeID) = 
    tryHash 0 (Ln nodeID) exprMap

mult : List RawExpr -> RawExpr
mult rawExprs = 
    case rawExprs of
        one::[] -> one
        _ ->
            let
                sortedExprs = List.sortBy (\(_, nodeID) -> nodeID) rawExprs
                (exprMap, nodeIDs) = List.unzip sortedExprs
                unionizedMap = List.foldl mergeAndRehash Dict.empty exprMap
            in
                tryHash 0 (Mult nodeIDs) unionizedMap

add : List RawExpr -> RawExpr
add rawExprs = 
    case rawExprs of
        one::[] -> one
        _ ->
            let
                sortedExprs = List.sortBy (\(_, nodeID) -> nodeID) rawExprs
                (exprMap, nodeIDs) = List.unzip sortedExprs
                unionizedMap = List.foldl mergeAndRehash Dict.empty exprMap
            in
                tryHash 0 (Add nodeIDs) unionizedMap

neg : RawExpr -> RawExpr
neg (exprMap, nodeID) = 
    tryHash 0 (Neg nodeID) exprMap

-- func : String -> List String -> RawExpr -> RawExpr
-- func name args (exprMap, nodeID) = 
--     tryHash 0 (Func name args nodeID) exprMap

let_ : String -> RawExpr -> RawExpr -> RawExpr
let_ v (exprMap0, nodeID0) (exprMap1, nodeID1) = 
    let
        unionizedMap = mergeAndRehash exprMap0 exprMap1
    in
        tryHash 0 (Let (v, nodeID0) nodeID1) unionizedMap

toString e =
    case e of
        Const f -> "Const \"" ++ (String.fromFloat f) ++ "\""
        Var s -> "Var \"" ++ s ++ "\""
        Sqrt n -> "Sqrt \"" ++ n ++ "\""
        IntPow n i -> "IntPow \"" ++ n ++ "\" " ++ (String.fromInt i)
        Exp n -> "Exp \"" ++ n ++ "\""
        Ln n -> "Ln \"" ++ n ++ "\""
        Mult ns -> "Mult [" ++ String.join "," ns ++ "]"
        Add ns -> "Add [" ++ String.join "," ns ++ "]"
        Neg n -> "Neg \"" ++ n ++ "\""
        Let (v, nodeId) n -> "Let (" ++ v ++ "," ++ nodeId ++ ") \"" ++ n ++ "\""


mergeAndRehash : ExpressionMap -> ExpressionMap -> ExpressionMap
mergeAndRehash map1 map2 =
    let
        newDict = Dict.merge 
                Dict.insert 
                (\ k a b d -> 
                    if a == b then
                        Dict.insert k a d
                    else
                        let
                            hash = tryHash 0 a d |> Tuple.second
                        in
                            Dict.insert hash a d
                )
                Dict.insert
                map1 map2 Dict.empty
    in
        newDict

tryHash : Int -> HashExpr -> ExpressionMap -> RawExpr
tryHash n hashedExpr hashTable =
    case Dict.get (hashExpr n hashedExpr) hashTable of
        Just foundExpr -> 
            if foundExpr == hashedExpr then
                (hashTable, hashExpr n hashedExpr)
            else
                tryHash (n+1) hashedExpr hashTable
        Nothing -> (Dict.insert (hashExpr n hashedExpr) hashedExpr hashTable, hashExpr n hashedExpr)


hashExpr : Int -> HashExpr -> NodeID
hashExpr n hashedExpr = (String.left 8 <| Sha256.sha256 <| toString hashedExpr) ++ (String.fromInt n)

test = add [const 1, const 2]

expr : Parser RawExpr
expr = 
 Parser.oneOf
    [ 
      loop [] expLoopHelper
    ]

expLoopHelper : List RawExpr -> Parser (Step (List RawExpr) RawExpr)
expLoopHelper revExprs =
  oneOf
    [ 
      succeed (\e -> Loop (e :: revExprs))
        |. spaces
        |= term
        |. spaces
    , succeed (\e -> Loop (e :: revExprs))
        |. spaces
        |. symbol "+"
        |. spaces
        |= term
        |. spaces
    , succeed (\e -> Loop (neg e :: revExprs))
        |. spaces
        |. symbol "-"
        |. spaces
        |= term
        |. spaces
    , case revExprs of
        first::rest ->
          succeed ()
              |> Parser.map (\_ -> Done <| add <| first :: rest)
        _ -> 
          problem "Not an expression"
    ]

{- 

term -> factor { '*' factor } | factor '^' int

-}
term : Parser RawExpr
term = 
 Parser.oneOf
    [ loop [] termLoopHelper
    , succeed intPow
        |= factor     
        |. spaces
        |. symbol "^"
        |. spaces
        |= Parser.int
    ]
    
termLoopHelper : List RawExpr -> Parser (Step (List RawExpr) RawExpr)
termLoopHelper revExprs =
  oneOf
    [ succeed (\e -> Loop (e :: revExprs))
        |. spaces
        |= factor
        |. spaces
    , succeed (\e -> Loop (e :: revExprs))
        |. symbol "*"
        |. spaces
        |= factor
        |. spaces
    , case revExprs of
        first::rest ->
          succeed ()
              |> Parser.map (\_ -> Done <| mult <| first :: rest)
        _ -> 
          problem "Not a term"
    ]

{- 

factor -> float | var | 'sqrt' factor | 'log' factor | 'exp' factor | '(' exp ')'

-}
factor : Parser RawExpr
factor =
  Parser.oneOf
    [ succeed const
        |= Parser.float
    , succeed var
        |= Parser.variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "let", "in", "sqrt", "exp", "log" ]
            }
    , succeed sqrt
        |. symbol "sqrt"
        |. spaces
        |= lazy ( \ () -> factor )
        |. spaces
    , succeed ln
        |. symbol "log"
        |. spaces
        |= lazy ( \ () -> factor )
        |. spaces
    , succeed exp
        |. symbol "exp"
        |. spaces
        |= lazy ( \ () -> factor )
        |. spaces
    , succeed identity
        |. symbol "("
        |. spaces
        |= expr
        |. spaces
        |. symbol ")"
    , succeed let_
        |. symbol "let"
        |. spaces
        |= Parser.variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "let", "in", "sqrt", "exp", "log" ]
            }
        |. spaces
        |. symbol "="
        |. spaces
        |= expr
        |. spaces
        |. symbol "in"
        |. spaces
        |= expr
    , problem "not a constant or variable"
    ]
    
exprParser : Parser RawExpr
exprParser =
  succeed identity
    |= expr
    |. end

myParser =
  exprParser

-- Your initial test text
myTestText = "sqrt 2*3+4"


evalExpr : Dict String Float -> RawExpr -> Result String (Float, Int)
evalExpr env (exprMap, nodeId) = 
    eval env 0 exprMap nodeId

evalMemoExpr : Dict String Float -> RawExpr -> Result String (Float, Int)
evalMemoExpr env (exprMap, nodeId) = 
    evalMemo env (Dict.empty, 0) exprMap nodeId 
        |> Result.map (\(f, (_, n)) -> (f, n))

type alias MemoMap = Dict String Float

-- evalMemo : Dict String Float -> (MemoMap, Int) -> ExpressionMap -> NodeID -> Result String (Float, (MemoMap, Int))
-- evalMemo env (memoMap, n) exprMap nodeId = 
--     case Dict.get nodeId exprMap of
--         Just (Let (v, e) body) ->
--             case evalMemo env (memoMap, n) exprMap e of
--                 Ok (f, (_, nn)) -> evalMemo (Dict.insert v f env) (Dict.empty, nn+1) exprMap body
--                 Err s -> Err s
--         Just _ -> 
--             case Dict.get nodeId memoMap of
--                 Just f -> Ok (f, (memoMap, n))
--                 Nothing -> 
--                     case eval env n exprMap nodeId of
--                         Ok (f, nn) -> 
--                             let
--                                 newMemoMap = Dict.insert nodeId f memoMap
--                             in
--                                 Ok (f, (newMemoMap, nn+1))
--                         Err s -> Err s
--         Nothing ->
--             Err <| "Node " ++ nodeId ++ " not found in expression map"

evalMemo : Dict String Float -> (MemoMap, Int) -> ExpressionMap -> NodeID -> Result String (Float, (MemoMap, Int))
evalMemo env (memoMap, n) exprMap nodeId = 
    case Dict.get nodeId memoMap of
        Just f -> Ok (f, (memoMap, n))
        Nothing ->
            case Dict.get nodeId exprMap of
                Just (Const f) -> 
                    let
                        newMemoMap = Dict.insert nodeId f memoMap
                    in
                        Ok (f, (newMemoMap, n+1))
                Just (Let (v, e) body) ->
                    case eval env n exprMap e of
                        Ok (f, nn) -> evalMemo (Dict.insert v f env) (Dict.empty, nn+1) exprMap body
                        Err s -> Err s
                Just (Var s) -> 
                    case Dict.get s env of
                        Just f -> 
                            let
                                newMemoMap = Dict.insert nodeId f memoMap
                            in
                                Ok (f, (newMemoMap, n+1))
                        Nothing -> Err <| "Variable " ++ s ++ " not found in environment"
                Just (Sqrt e) -> 
                    case evalMemo env (memoMap, n) exprMap e of
                        Ok (f, (_, nn)) -> 
                            let
                                newMemoMap = Dict.insert nodeId (Basics.sqrt f) memoMap
                            in
                                Ok (Basics.sqrt f, (newMemoMap, nn+1))
                        Err s -> Err s
                Just (Ln e) -> 
                    case evalMemo env (memoMap, n) exprMap e of
                        Ok (f, (_, nn)) -> 
                            let
                                newMemoMap = Dict.insert nodeId (Basics.logBase 2 f) memoMap
                            in
                                Ok (Basics.logBase 2 f, (newMemoMap, nn+1))
                        Err s -> Err s
                Just (Exp e) -> 
                    case evalMemo env (memoMap, n) exprMap e of
                        Ok (f, (_, nn)) -> 
                            let
                                newMemoMap = Dict.insert nodeId (Basics.e ^ f) memoMap
                            in
                                Ok (Basics.e ^ f, (newMemoMap, nn+1))
                        Err s -> Err s
                Just (Add es) ->
                    List.foldl 
                        (\e acc -> 
                            case acc of
                                Ok (f, (newMemoMap, nn)) -> 
                                    case evalMemo env (newMemoMap, nn) exprMap e of
                                        Ok (ff, (newMemoMap2, nnn)) -> Ok (f + ff, (newMemoMap2, nnn))
                                        Err s -> Err s
                                Err s -> Err s
                        )
                        (Ok (0, (memoMap, n + List.length es - 1)))
                        es
                Just (Mult es) ->
                    List.foldl 
                        (\e acc -> 
                            case acc of
                                Ok (f, (newMemoMap, nn)) -> 
                                    case evalMemo env (newMemoMap, nn) exprMap e of
                                        Ok (ff, (newMemoMap2, nnn)) -> Ok (f * ff, (newMemoMap2, nnn))
                                        Err s -> Err s
                                Err s -> Err s
                        )
                        (Ok (1, (memoMap, n + List.length es - 1)))
                        es
                Just (IntPow e i) ->
                    case evalMemo env (memoMap, n) exprMap e of
                        Ok (f, (_, nn)) -> 
                            let
                                newMemoMap = Dict.insert nodeId (f ^ toFloat i) memoMap
                            in
                                Ok (f ^ toFloat i, (newMemoMap, nn+1))
                        Err s -> Err s
                Just (Neg e) ->
                    case evalMemo env (memoMap, n) exprMap e of
                        Ok (f, (_, nn)) -> 
                            let
                                newMemoMap = Dict.insert nodeId (-f) memoMap
                            in
                                Ok (-f, (newMemoMap, nn+1))
                        Err s -> Err s
                Nothing -> 
                    Err <| "Node " ++ nodeId ++ " not found in expression map"

                        
eval : Dict String Float -> Int -> ExpressionMap -> NodeID -> Result String (Float, Int)
eval env n exprMap nodeId = 
    case Dict.get nodeId exprMap of
        Just (Const f) -> Ok (f, n+1)
        Just (Var s) -> 
            case Dict.get s env of
                Just f -> Ok (f, n+1)
                Nothing -> Err <| "Variable " ++ s ++ " not found in environment"
        Just (Sqrt e) -> 
            case eval env n exprMap e of
                Ok (f,nn) -> Ok (Basics.sqrt f, nn+1)
                Err s -> Err s
        Just (Ln e) -> 
            case eval env n exprMap e of
                Ok (f, nn) -> Ok (Basics.logBase 2 f, nn)
                Err s -> Err s
        Just (Exp e) -> 
            case eval env n exprMap e of
                Ok (f, nn) -> Ok (Basics.e ^ f, nn)
                Err s -> Err s
        Just (Add es) ->
            case foldResults (List.map (eval env 0 exprMap) es) of
                Ok fs -> 
                    let (ffs, ns) = List.unzip fs
                    in 
                        Ok (List.sum ffs, List.sum ns + List.length es - 1)
                Err s -> Err s
        Just (Mult es) ->
            case foldResults (List.map (eval env 0 exprMap) es) of
                Ok fs -> 
                    let (ffs, ns) = List.unzip fs
                    in 
                        Ok (List.product ffs, List.sum ns + List.length es - 1)
                Err s -> Err s
        Just (IntPow e i) ->
            case eval env n exprMap e of
                Ok (f, nn) -> Ok (f ^ (toFloat i), nn + 1)
                Err s -> Err s
        Just (Neg e) ->
            case eval env n exprMap e of
                Ok (f, nn) -> Ok (-f, nn+1)
                Err s -> Err s
        Just (Let (v, e) body) ->
            case eval env n exprMap e of
                Ok (f, nn) -> eval (Dict.insert v f env) (nn + 1) exprMap body
                Err s -> Err s
        Nothing -> 
            Err <| "Node " ++ nodeId ++ " not found in expression map"

foldResults : List (Result x a) -> Result x (List a)
foldResults rs =
    case rs of
        [] -> Ok []
        r::rS ->
            case r of
                Ok a -> 
                    case foldResults rS of
                        Ok aS -> Ok <| a :: aS
                        Err s -> Err s
                Err s -> Err s

-- The view function
view model = 
  let
        env = Parser.run environmentParser model.environmentText
  in 
  Html.div [] 
    [
      Html.h4 [] [Html.text "Input"]
    , Html.text "Type here to test different inputs"
    , Html.div[] [Html.textarea [A.value model.text, E.onInput NewString, A.rows 3] []]
    , Html.div[] [Html.textarea [A.value model.environmentText, E.onInput NewEnvironment, A.rows 3] []]
    
    , Html.h4 [] [Html.text "Enviroment"]
    , Html.div [] [Html.text <| Debug.toString env]
    
    , Html.h4 [] [Html.text "Parser Output"]
    , Html.div [] [Html.text <| Debug.toString <| Parser.run myParser model.text]
    , Html.h4 [] [Html.text "Non-Memoized Evaluation Output"]
    , case env of
        Ok goodEnv -> 
            case Result.map (evalExpr goodEnv) <| Parser.run myParser model.text of
                Ok (Ok (f, n)) ->
                    Html.div [] 
                        [ Html.text <| "Result: " ++ Debug.toString f
                        , Html.br [] []
                        , Html.text <| "Total calculations: " ++ Debug.toString n
                        ]
                Ok (Err s) -> Html.text s
                Err s -> Html.text <| Debug.toString s
        _ -> Html.text "Could not parse environment"
    , Html.h4 [] [Html.text "Memoized Evaluation Output"]
    , case env of
        Ok goodEnv -> 
            case Result.map (evalMemoExpr goodEnv) <| Parser.run myParser model.text of
                Ok (Ok (f, n)) ->
                    Html.div [] 
                        [ Html.text <| "Result: " ++ Debug.toString f
                        , Html.br [] []
                        , Html.text <| "Total calculations: " ++ Debug.toString n
                        ]
                Ok (Err s) -> Html.text s
                Err s -> Html.text <| Debug.toString s
        _ -> Html.text "Could not parse environment"
    -- , Html.pre [] [Html.text <| myDebugText model] --[Html.textarea [A.value myDebugText, A.rows 10] []]
    ]

-- enviroment is a string like
--  x = 2
--  y = 3
environmentParser : Parser (Dict String Float)
environmentParser =
    Parser.map Dict.fromList <| Parser.sequence 
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item = varParser
        , trailing = Parser.Optional -- demand a trailing semi-colon
        }

varParser : Parser (String, Float)
varParser = 
    succeed Tuple.pair
        |. spaces
        |= Parser.variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "let", "in", "sqrt", "exp", "log" ]
            }
        |. spaces
        |. symbol "="
        |. spaces
        |= Parser.float
        |. spaces

type Msg = NewString String | NewEnvironment String

type alias Model = { text: String }

update msg model = case msg of
                    NewString newText -> { model | text = newText }
                    NewEnvironment newEnv -> { model | environmentText = newEnv }

init = { text = "sqrt 2 + 3", environmentText = "x = 2\ny = 3" }

main = Browser.sandbox { init = init, update = update, view = view }