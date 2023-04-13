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
    let
        hash = hashExpr <| Const f
    in
        (Dict.singleton hash (Const f), hash)

var : String -> RawExpr
var s =
    let
        hash = hashExpr <| Var s
    in
        (Dict.singleton hash (Var s), hash)

sqrt : RawExpr -> RawExpr
sqrt (exprMap, nodeID) = 
    let
        hash = hashExpr <| Sqrt nodeID
    in
        (Dict.insert hash (Sqrt nodeID) exprMap, hash)

intPow : RawExpr -> Int -> RawExpr
intPow (exprMap, nodeID) i = 
    let
        hash = hashExpr <| IntPow nodeID i
    in
        (Dict.insert hash (IntPow nodeID i) exprMap, hash)

exp : RawExpr -> RawExpr
exp (exprMap, nodeID) = 
    let
        hash = hashExpr <| Exp nodeID
    in
        (Dict.insert hash (Exp nodeID) exprMap, hash)

ln : RawExpr -> RawExpr
ln (exprMap, nodeID) = 
    let
        hash = hashExpr <| Ln nodeID
    in
        (Dict.insert hash (Ln nodeID) exprMap, hash)

mult : List RawExpr -> RawExpr
mult rawExprs = 
    case rawExprs of
        one::[] -> one
        _ ->
            let
                sortedExprs = List.sortBy (\(_, nodeID) -> nodeID) rawExprs
                (exprMap, nodeIDs) = List.unzip sortedExprs
                hash = hashExpr <| Mult nodeIDs
            in
                (Dict.insert hash (Mult nodeIDs) (List.foldl Dict.union Dict.empty exprMap), hash)

add : List RawExpr -> RawExpr
add rawExprs = 
    case rawExprs of
        one::[] -> one
        _ ->
            let
                sortedExprs = List.sortBy (\(_, nodeID) -> nodeID) rawExprs
                (exprMap, nodeIDs) = List.unzip sortedExprs
                hash = hashExpr <| Add nodeIDs
            in
                (Dict.insert hash (Add nodeIDs) (List.foldl Dict.union Dict.empty exprMap), hash)

neg : RawExpr -> RawExpr
neg (exprMap, nodeID) = 
    let
        hash = hashExpr <| Neg nodeID
    in
        (Dict.insert hash (Neg nodeID) exprMap, hash)
    

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


hashExpr : HashExpr -> NodeID
hashExpr = String.left 8 << Sha256.sha256 << toString

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