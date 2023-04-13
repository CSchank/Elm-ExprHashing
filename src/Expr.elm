module Expr exposing (..)

import Set
import Parser exposing (..)
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import Result exposing (Result(..))
import Browser
import HashExpr exposing(hashExpr)

import Dict exposing (Dict)
import Set exposing (Set)

type Expr = Const Float
          | Var String -- in math we often use Char
          | Sqrt Expr -- we can take sqrt(anything)
          | IntPow Expr Int -- the easy case of exponent
          | Exp Expr -- e^expr
          | Ln Expr
          | Mult Expr Expr
          | Add Expr Expr
          | Neg Expr
          
x = Var "x"
y = Var "y"
z = Var "z"

myEnv = Dict.fromList [("x",3),("y",4),("z",5)]

example = Add (Const 7) (Mult (Const 4) (Add (Sqrt x) (Exp y)) )
example2 = Add (Var "x") (Var "y")
example3 = Mult (Var "z") (Const 0)
example4 = Add (Sqrt (Mult (Var "x") (Const 0))) (Var "y")
example5 = (Sqrt ( (Const 0)))
example4Again = Add (example5) (Var "y")

pretty e = case e of
            (Var name) -> name
            (Mult expr1 expr2) -> "(" ++ (pretty expr1) ++ ")" ++ " * " ++ "(" ++ (pretty expr2) ++ ")"
            (Sqrt expr) -> "sqrt(" ++ (pretty expr) ++ ")"
            (Const c) -> String.fromFloat c
            (IntPow expr exponent) -> "(" ++ (pretty expr) ++ ")^(" ++ (String.fromInt exponent)++")"
            (Exp expr) -> "e^("++ (pretty expr) ++ ")"
            (Ln expr) -> "ln("++pretty expr++ ")"
            (Add expr1 expr2) -> (pretty expr1) ++ " + " ++ (pretty expr2)
            (Neg expr) -> "- (" ++ pretty expr ++ ")"

eval : Dict String Float -> Expr -> Result String Float
eval env expr =
  case expr of
    (Var name) -> case Dict.get name env of
                    Just val -> Result.Ok val
                    Nothing  -> Result.Err ( name ++ " undefined" )
    (Mult expr1 expr2) -> 
      case (eval env expr1, eval env expr2) of
        (Result.Ok val1, Result.Ok val2) -> Result.Ok (val1 * val2)
        (Result.Err e, _)                -> Result.Err e
        (_,Result.Err e)                 -> Result.Err e
    (Sqrt expr1) -> 
      case eval env expr1 of
        Ok r -> 
          if r >= 0 then 
            Result.Ok <| sqrt r
          else
            Result.Err "Cannot take the square root of a negative number"
        Err e ->
          Err <| "In square root: " ++ e
    (Const c) -> Result.Ok c
    (IntPow expr1 exponent) -> 
      Result.map (\r -> r ^ (toFloat exponent)) <| eval env expr1
    (Exp expr1) -> 
      Result.map (\r -> e ^ r) <| eval env expr1
    (Ln expr1) ->  
      Result.map (logBase e) <| eval env expr1
    (Add expr1 expr2) -> 
      Result.map2 (+) (eval env expr1) (eval env expr2)
    (Neg expr1) ->
      Result.map negate <| eval env expr1

    
codeGen : Expr -> String
codeGen expr =
  let
    genInputParam n = ["(param $", n, " f64)"] |> String.concat
    inputParams = String.join " " <| List.map genInputParam <| Set.toList <| findVars expr
    moduleHeader = "(module"
    imports = 
      ["  (func $intpow (import \"imports\" \"intpow\") (param i32))"
      ,"  (func $exp (import \"imports\" \"exp\") (param i32))"
      ,"  (func $ln (import \"imports\" \"ln\") (param i32))"
      ]
    funcHeader = ["  (func ", inputParams, " (result f64)"] |> String.concat
    body expr0 =
      case expr0 of
        (Var name) -> 
          ["local.get $" ++ name]
        (Mult expr1 expr2) -> 
          body expr1 ++ body expr2 ++ ["f64.mult"]
        (Sqrt expr1) -> 
          body expr1 ++ ["f64.sqrt"]
        (Const c) -> 
          ["f64.const " ++ String.fromFloat c]
        (IntPow expr1 exponent) -> 
          ["i64.const " ++ String.fromInt exponent]
          ++ body expr1 ++
          ["call $intpow"]
        (Exp expr1) -> 
          body expr1 ++ ["call $exp"]
        (Ln expr1) -> 
          body expr1 ++ ["call $ln"]
        (Add expr1 expr2) ->
          body expr1 ++ body expr2 ++ ["f64.add"]
        (Neg expr1) ->
          body expr1 ++ ["f64.neg"]
  in
    String.join "\n" <|
    [moduleHeader] ++ imports ++ [funcHeader] ++
    (List.map (\str -> "    " ++ str) (body expr)) ++ ["    )", ")"]

findVars : Expr -> Set String
findVars expr =
  case expr of
    (Var name) -> 
      Set.singleton name
    (Mult expr1 expr2) -> 
      findVars expr1 |> Set.union (findVars expr2)
    (Sqrt expr1) -> 
      findVars expr1
    (Const c) -> 
      Set.empty
    (IntPow expr1 exponent) -> 
      findVars expr1
    (Exp expr1) -> 
      findVars expr1
    (Ln expr1) -> 
      findVars expr1
    (Add expr1 expr2) -> 
      findVars expr1 |> Set.union (findVars expr2)
    (Neg expr1) -> 
      findVars expr1


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