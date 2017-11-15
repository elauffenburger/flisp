module Flisp.Interpreter.Eval

open Flisp.Syntax.Common
open Flisp.Syntax

let handleProcResult result =
    match result with
    | Error err -> failwith err
    | Success cell -> cell

let rec eval services expr env =
    let evalWithServices cell = eval services cell env
    let applyWithServices fn args = 
        services.apply services fn.name fn args fn.env 
        |> handleProcResult

    match expr with
    | Symbol sym -> ExecEnv.forceResolveSymbol sym env
    | Number _ | String _ -> expr
    | Quote quoted -> quoted
    | Lispt cells -> match cells with
        | [] -> Lispt []
        | [x] -> Function.forceInvoke applyWithServices x
        | x::xs -> Function.forceInvokeWithArgs services (evalWithServices x) xs env |> handleProcResult

let apply services fnName fn args env =
    let newEnv = ExecEnv.makeChild env

    let eval env cell = services.eval services cell env
            
    let evalArgAndAddToEnv env (arg, paramName) =
        // eval the arg, then add it to the execution env as the parameter name
        let evaldArg = eval env arg
        ExecEnv.addOrUpdate paramName evaldArg newEnv

    Function.forceParamNames fn
    |> List.zip args
    |> List.iter (evalArgAndAddToEnv env)

    Cell.forceToList fn.body
    |> List.map (eval newEnv)
    |> List.last
    |> Success