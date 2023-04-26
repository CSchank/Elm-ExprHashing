module HashExpr exposing (..)

import Set
import Dict exposing (Dict)
import Sha256
import Browser
import Html
import Html.Attributes as A
import Html.Events as E
import Parser exposing(Parser, Step, oneOf, succeed, (|=), (|.), end, symbol, spaces, lazy, problem, Step(..), loop)

type HashExpr = Const Float
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
        -- Func s ns n -> "Func \"" ++ s ++ "\" [" ++ String.join "," ns ++ "] \"" ++ n ++ "\""
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

type alias NodeID = String
type alias ExpressionMap = Dict NodeID HashExpr

type alias RawExpr = (ExpressionMap, NodeID)

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


evalExpr : Dict String Float -> RawExpr -> Result String Float
evalExpr env (exprMap, nodeId) = 
    eval env exprMap nodeId

eval : Dict String Float -> ExpressionMap -> NodeID -> Result String Float
eval env exprMap nodeId = 
    case Dict.get nodeId exprMap of
        Just (Const f) -> Ok f
        Just (Var s) -> 
            case Dict.get s env of
                Just f -> Ok f
                Nothing -> Err <| "Variable " ++ s ++ " not found in environment"
        Just (Sqrt e) -> 
            case eval env exprMap e of
                Ok f -> Ok <| Basics.sqrt f
                Err s -> Err s
        Just (Ln e) -> 
            case eval env exprMap e of
                Ok f -> Ok <| Basics.logBase 2 f
                Err s -> Err s
        Just (Exp e) -> 
            case eval env exprMap e of
                Ok f -> Ok <| Basics.e ^ f
                Err s -> Err s
        Just (Add es) ->
            case foldResults (List.map (eval env exprMap) es) of
                Ok fs -> Ok <| List.sum fs
                Err s -> Err s
        Just (Mult es) ->
            case foldResults (List.map (eval env exprMap) es) of
                Ok fs -> Ok <| List.product fs
                Err s -> Err s
        Just (IntPow e i) ->
            case eval env exprMap e of
                Ok f -> Ok <| f ^ (toFloat i)
                Err s -> Err s
        Just (Neg e) ->
            case eval env exprMap e of
                Ok f -> Ok <| -f
                Err s -> Err s
        Just (Let (v, e) body) ->
            case eval env exprMap e of
                Ok f -> eval (Dict.insert v f env) exprMap body
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
        env = Dict.fromList [("x", 2), ("y", 3)]
  in 
  Html.div [] 
    [
      Html.h4 [] [Html.text "Input"]
    , Html.text "Type here to test different inputs"
    , Html.div[] [Html.textarea [A.value model.text, E.onInput NewString, A.rows 3] []]
    , Html.h4 [] [Html.text "Parser Output"]
    , Html.div [] [Html.text <| Debug.toString <| Parser.run myParser model.text]
    , Html.div [] [Html.text <| Debug.toString <| Result.map (evalExpr env) <| Parser.run myParser model.text]
    -- , Html.pre [] [Html.text <| myDebugText model] --[Html.textarea [A.value myDebugText, A.rows 10] []]
    ]


type Msg = NewString String

type alias Model = { text: String }

update msg model = case msg of
                    NewString newText -> { model | text = newText }

init = { text = "sqrt 2 + 3" }

main = Browser.sandbox { init = init, update = update, view = view }