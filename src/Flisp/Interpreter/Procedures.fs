module Flisp.Interpreter.Procedures

open System.Collections.Generic
open Flisp.Syntax.Common
open Flisp.Interpreter.Eval
open Flisp.Syntax

let print cells env =
    match cells with
    | [] -> Success (Symbol "nil")
    | _ -> 
        printfn "%A" cells 
        Success (Symbol "nil")

let add cells env =
    match cells with 
    | [Number x; Number y] -> Success (Number (x+y))
    | _ -> Error "incorrect signature for add"

let map cells env =
    match cells with

    // We're looking for a very specific signature for map
    | [Lambda ({ parms = [Symbol paramName]; body = fn}); Lispt items] ->
        // The functor
        let iterMap item = 
            let newEnv = ExecEnv.makeChild env

            // Add functor parameter to environment
            ExecEnv.addOrUpdate paramName item newEnv

            // eval fn 
            let res = newExpr fn newEnv |> eval

            match res with
            | [x] -> x
            | [] -> Symbol "nil"
            | _ -> Lispt res

        // Map over items
        let result = List.map iterMap items
        
        // Return successful result as a Lispt
        result |> Lispt |> Success

    | _ -> Error "incorrect signature for map"

let define cells env =
    match cells with
    | [Symbol symbol; (value: Cell)] ->
        // Update the environment with the new symbol

        let evaluatedValue = match eval <| newExpr [value] env with
            | [x] -> x
            | xs -> Lispt xs

        ExecEnv.addOrUpdate symbol evaluatedValue <| ExecEnv.parentOrSelf env

        Success value
    | _ -> Error "Wrong signature for define"

let makeDefaultEnv() =
    let data = dict [
        "nil", Value nil;
        "map", Procedure (new Proc(map));
        "print", Procedure (new Proc(print));
        "define", SpecialForm (new Proc(define));
        "+", Procedure (new Proc(add))
    ]

    ExecEnv.make data

let defaultExpr cells = newExpr cells <| makeDefaultEnv()
