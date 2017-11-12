module Flisp.Interpreter.Procedures

open System.Collections.Generic
open Flisp.Syntax.Common
open Flisp.Interpreter.Eval
open Flisp.Syntax

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
    | [Function ({ parms = [Symbol paramName]; body = fn}); Lispt items] ->
        // The functor
        let iterMap item = 
            let newEnv = ExecEnv.makeChild env

            // Add functor parameter to environment
            ExecEnv.addOrUpdate paramName item newEnv

            // eval fn 
            let res = newExpr fn newEnv |> (eval services)

            match res with
            | [x] -> x
            | [] -> Symbol "nil"
            | _ -> Lispt res

        // Map over items
        let result = List.map iterMap items
        
        // Return successful result as a Lispt
        Success <| Lispt result

    | _ -> Error "incorrect signature for map"

let define services cells env =
    match cells with
    | [Symbol symbol; (value: Cell)] ->
        // Update the environment with the new symbol

        let evaluatedValue = match (eval services) <| newExpr [value] env with
            | [x] -> x
            | xs -> Lispt xs

        ExecEnv.addOrUpdate symbol evaluatedValue <| ExecEnv.parentOrSelf env

        Success value
    | _ -> Error "Wrong signature for define"

let funcall services cells env : ProcResult = 
    let invoke fnName fn args env =
        let newEnv = ExecEnv.makeChild env

        let evalArgAndAddToEnv env (arg, paramName) =
            // eval the arg, then add it to the execution env as the parameter name
            let execdArg = eval services <| newExpr args env |> Cell.fromList
            ExecEnv.addOrUpdate paramName execdArg env

        Function.forceParamNames fn
        |> List.zip args
        |> List.iter (evalArgAndAddToEnv env)

        eval services <| newExpr fn.body newEnv
        |> Cell.fromList
        |> Success
    
    // Make sure the call signature for funcall matches funcall
    match cells with
    | [Symbol fnName; Lispt args] ->
        match Function.validate fnName args env with
        | Function.ValidationResult.Success (fnName, fn, args, env) -> invoke fnName fn args env
        | Function.ValidationResult.Error _ -> ProcResult.Error "Validation failed"

    | _ -> Error "Wrong signature for funcall"

let makeDefaultEnv() =
    let data = dict [
        "nil", Value nil;
        "map", Procedure map;
        "print", Procedure print;
        "define", MetaProcedure define;
        "funcall", MetaProcedure funcall;
        "+", Procedure add
    ]

    ExecEnv.make data

let defaultExpr cells = newExpr cells <| makeDefaultEnv()
