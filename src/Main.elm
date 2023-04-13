module Main exposing(..)

import Set
import Parser exposing (..)
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import Result exposing (Result(..))
import Browser
import HashExpr exposing(hashExpr)
import Expr exposing(..)

{- 

exp -> term { ('+' | '-') term }

-}
exp : Parser Expr
exp = 
 Parser.oneOf
    [ 
      loop [] expLoopHelper
    ]

expLoopHelper : List Expr -> Parser (Step (List Expr) Expr)
expLoopHelper revExprs =
  oneOf
    [ 
      succeed (\e -> Loop (e :: revExprs))
        |= term
        |. spaces
    , succeed (\e -> Loop (e :: revExprs))
        |. spaces
        |. symbol "+"
        |. spaces
        |= term
        |. spaces
    , succeed (\e -> Loop (Neg e :: revExprs))
        |. spaces
        |. symbol "-"
        |. spaces
        |= term
        |. spaces
    , case revExprs of
        first::rest ->
          succeed ()
              |> Parser.map (\_ -> Done <| List.foldl Add first rest)
        _ -> 
          problem "Not an expression"
    ]

{- 

term -> factor { '*' factor } | factor '^' int

-}
term : Parser Expr
term = 
 Parser.oneOf
    [ loop [] termLoopHelper
    , succeed IntPow
        |= factor     
        |. spaces
        |. symbol "^"
        |. spaces
        |= Parser.int
    ]
    
termLoopHelper : List Expr -> Parser (Step (List Expr) Expr)
termLoopHelper revExprs =
  oneOf
    [ succeed (\e -> Loop (e :: revExprs))
        |= factor
    , succeed (\e -> Loop (e :: revExprs))
        |. symbol "*"
        |. spaces
        |= factor
        |. spaces
    , case revExprs of
        first::rest ->
          succeed ()
              |> Parser.map (\_ -> Done <| List.foldl Mult first rest)
        _ -> 
          problem "Not a term"
    ]

{- 

factor -> float | var | 'sqrt' factor | 'log' factor | 'exp' factor | '(' exp ')'

-}
factor : Parser Expr
factor =
  Parser.oneOf
    [ succeed Const
        |= Parser.float
    , succeed Var
        |= Parser.variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "let", "in", "sqrt", "exp", "log" ]
            }
    , succeed Sqrt
        |. symbol "sqrt"
        |. spaces
        |= lazy ( \ () -> factor )
        |. spaces
    , succeed Ln
        |. symbol "log"
        |. spaces
        |= lazy ( \ () -> factor )
        |. spaces
    , succeed Exp
        |. symbol "exp"
        |. spaces
        |= lazy ( \ () -> factor )
        |. spaces
    , succeed identity
        |. symbol "("
        |. spaces
        |= exp
        |. spaces
        |. symbol ")"
    , problem "not a constant or variable"
    ]
    
exprParser : Parser Expr
exprParser =
  succeed identity
    |= exp
    |. end

myParser =
  exprParser

-- Your initial test text
myTestText = "sqrt 2*3+4"

testExpr = example
-- Your debug test string
myDebugText model = 
  case Parser.run myParser model.text of
    Ok testExpr0 ->
      "Expr: " ++ Debug.toString testExpr0 ++ "\n"
        ++ "Pretty expr: " ++ pretty testExpr0
        ++ "\nWASM code:\n" ++ codeGen testExpr0
    Err e -> Debug.toString e
-- Your top-level parser to test





-- The view function
view model = 
  Html.div [] 
    [
      Html.h4 [] [Html.text "Input"]
    , Html.text "Type here to test different inputs"
    , Html.div[] [Html.textarea [A.value model.text, E.onInput NewString, A.rows 3] []]
    , Html.h4 [] [Html.text "Parser Output"]
    , Html.div [] [Html.text <| Debug.toString <| Parser.run myParser model.text]
    , Html.h4 [] [Html.text "Hash"]
    , Html.div [] [Html.text <| Debug.toString <| Result.map hashExpr <| Parser.run myParser model.text]
    , Html.h4 [] [Html.text "Debug Output"]
    , Html.pre [] [Html.text <| myDebugText model] --[Html.textarea [A.value myDebugText, A.rows 10] []]
    ]

type Msg = NewString String

type alias Model = { text: String }

update msg model = case msg of
                    NewString newText -> { model | text = newText }

init = { text = myTestText }

main = Browser.sandbox { init = init, update = update, view = view }