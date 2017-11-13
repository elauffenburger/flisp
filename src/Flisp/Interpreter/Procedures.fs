module Flisp.Interpreter.Procedures

open System.Collections.Generic
open Flisp.Syntax.Common
open Flisp.Interpreter.Eval
open Flisp.Syntax
open System.Text.RegularExpressions

let print services cells env =
    match cells with
    | [] -> Success (Symbol "nil")
    | _ -> 
        sprintf "%A" cells |> services.log 
        Success (Symbol "nil")

let add services cells env =
    match cells with 
    | [Number x; Number y] -> Success <| Number (x+y)
    | _ -> Error "incorrect signature for add"

let map services cells env =
    match cells with

    // We're looking for a very specific signature for map
    | [Function fn; Lispt items] ->
        // Map over items
        List.map (fun item -> apply services "" fn [item] fn.env) items
        |> List.fold (fun acc result -> acc @ [handleProcResult result]) []
        |> Cell.fromList
        |> Success

    | _ -> Error "incorrect signature for map"

let define services cells env =
    match cells with
    | [Symbol symbol; (value: Cell)] ->
        let evaluatedValue = match (eval services) <| newExpr [value] env with
            | [x] -> x
            | xs -> Lispt xs

        // Update the environment with the new symbol
        ExecEnv.addOrUpdate symbol evaluatedValue <| ExecEnv.parentOrSelf env

        Success value
    | _ -> Error "Wrong signature for define"

let lambda services cells env =
    match cells with
    | [Lispt parms; Lispt body] -> Function ({ parms = parms; body = body; env = env }) |> Success
    | _ -> Error "Wrong signature for lambda"

let funcall services cells env = 
    match cells with
    | [Symbol fnName; Lispt args] ->
        match Function.validate fnName args env with
        | Function.ValidationResult.Success (fnName, fn, args, env) -> 
            // eval arguments with the current environment, then apply function in its own environment
            apply services fnName fn (eval services <| newExpr args env) fn.env
        | Function.ValidationResult.Error err -> ProcResult.Error err 

    | _ -> Error "Wrong signature for funcall"

let makeDefaultEnv() =
    let data = dict [
        "nil", Value nil;
        "map", Procedure map;
        "print", Procedure print;
        "define", MetaProcedure define;
        "lambda", MetaProcedure lambda;
        "funcall", MetaProcedure funcall;
        "+", Procedure add
    ]

    ExecEnv.make data

let defaultExpr cells = newExpr cells <| makeDefaultEnv()
