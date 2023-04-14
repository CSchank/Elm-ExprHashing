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
                unionizedMap = List.foldl Dict.union Dict.empty exprMap
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
                unionizedMap = List.foldl Dict.union Dict.empty exprMap
            in
                tryHash 0 (Add nodeIDs) unionizedMap

neg : RawExpr -> RawExpr
neg (exprMap, nodeID) = 
    tryHash 0 (Neg nodeID) exprMap

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





-- The view function
view model = 
  Html.div [] 
    [
      Html.h4 [] [Html.text "Input"]
    , Html.text "Type here to test different inputs"
    , Html.div[] [Html.textarea [A.value model.text, E.onInput NewString, A.rows 3] []]
    , Html.h4 [] [Html.text "Parser Output"]
    , Html.div [] [Html.text <| Debug.toString <| Parser.run myParser model.text]
    -- , Html.pre [] [Html.text <| myDebugText model] --[Html.textarea [A.value myDebugText, A.rows 10] []]
    ]


type Msg = NewString String

type alias Model = { text: String }

update msg model = case msg of
                    NewString newText -> { model | text = newText }

init = { text = "sqrt 2 + 3" }

main = Browser.sandbox { init = init, update = update, view = view }