module Flisp.Interpreter.Procedures

open System.Collections.Generic
open Flisp.Syntax.Common
open Flisp.Interpreter.Eval
open Flisp.Syntax
open System.Text.RegularExpressions

let print services cells env =
    let printout arg = sprintf "%A" arg |> services.log 

    match cells with
    | [x] -> printout x
    | xs -> printout xs

    Success (Symbol "nil")

let add services cells env =
    match cells with 
    | [Number x; Number y] -> Success <| Number (x+y)
    | _ -> Error "incorrect signature for add"

let map services cells env =
    match cells with
    | x::xs -> match (x, xs) with
        | (Function fn, [Lispt items]) -> 
            // Map over items
            List.map (fun item -> apply services fn.name fn [item] fn.env) items
            |> List.fold (fun acc result -> acc @ [handleProcResult result]) []
            |> Cell.fromList
            |> Success

        | _ -> Error "incorrect signature for map"
    | _ -> Error "incorrect signature for map"

let define services cells env =
    match cells with
    | [Symbol symbol; (value: Cell)] ->
        let evaluatedValue = eval services value env

        // Update the environment with the new symbol
        ExecEnv.addOrUpdate symbol evaluatedValue env

        Success value
    | _ -> Error "Wrong signature for define"

let progn services cells env =
    let exec cell = services.eval services cell env

    match cells with
    | [Lispt exprs] -> List.map exec exprs |> List.last |> Success
    | _ -> Error "Wrong signature for progn"

let lambda services cells env =
    match cells with
    | [Lispt _ as parms; Lispt _ as body] -> Function ({ name = "lambda"; parms = parms; body = body; env = env }) |> Success
    | _ -> Error "Wrong signature for lambda"

let funcall services cells env = 
    let applyFn fnName fn args env = 
        // eval arguments with the current environment, then apply function in its own environment
        let evaldArgs = 
            Cell.toList args
            |> List.map (fun arg -> services.eval services arg env)

        apply services fnName fn evaldArgs fn.env

    match cells with
    | [Symbol fnName; Lispt _ as args] ->
        match Function.validate fnName args env with
        | Function.ValidationResult.Success (fnName, fn, args, env) -> applyFn fnName fn args env
        | Function.ValidationResult.Error err -> Error err 

    | _ -> Error "Wrong signature for funcall"

let makeDefaultEnv() =
    let data = dict [
        "nil", nil;
        "map", Procedure map;
        "print", Procedure print;
        "define", MetaProcedure define;
        "progn", MetaProcedure progn;
        "lambda", MetaProcedure lambda;
        "funcall", MetaProcedure funcall;
        "+", Procedure add
    ]

    ExecEnv.make data