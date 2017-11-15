module Flisp.Interpreter.Eval

open Flisp.Syntax.Common
open Flisp.Syntax

let newExpr cells env = { cells = cells; env = ExecEnv.makeChild env }

let handleProcResult result =
    match result with
    | Error err -> failwith err
    | Success cell -> cell

let rec eval services expr =
    let evalWithServices = eval services
    let evalRest rest = newExpr rest expr.env |> evalWithServices
    let evalList cells = evalRest cells

    let invokeProcedure proc args env = (proc services args env) |> handleProcResult 

    let funcall env = 
        let invokeProc2 proc env args = invokeProcedure proc args env
    
        match ExecEnv.resolveSymbol "funcall" env with
        | Some (MetaProcedure fn) -> invokeProc2 fn env
        | None -> failwith "missing env primitive funcall"

    let evalFn (fn: Function) args = [funcall fn.env args]

    let resolveSymbol sym success =
        match ExecEnv.resolveSymbol sym expr.env with
        | Some cell -> success cell
        | None -> sprintf "Symbol %A not found" sym |> failwith

    match expr.cells with
    | [] -> []
    | [x] ->
        match x with
        | Symbol sym -> resolveSymbol sym (fun cell -> [cell])
        | Lispt cells -> evalList cells
        | Value _ -> [x]
        | Number _ -> [x]
        | Function fn -> evalFn fn []
        | Procedure proc | MetaProcedure proc -> [invokeProcedure proc [] expr.env]
        | Quote cell -> [cell]

    | x::xs -> 
        match x with

        // Evaluate all inner expressions and then invoke
        | Procedure proc -> 
            let innerExpression = newExpr xs expr.env |> evalWithServices
            (proc services innerExpression expr.env) |> handleProcResult |> List.singleton

        // Examine the ast directly without evaluating inner expressions, then invoke 
        | MetaProcedure proc -> invokeProcedure proc xs expr.env |> List.singleton

        // Try to resolve the symbol and then reevaluate with the resolved result
        | Symbol sym -> resolveSymbol sym (fun cell -> evalWithServices ({ cells = cell::xs; env = expr.env }))

        // Evaluate the list and then the rest
        | Lispt cells -> evalList cells @ evalRest xs

        // Just hand back the value and evaluate the rest
        | Number _ | Value _ -> x :: evalRest xs

        // Unwrap the quoted contents and evaluate the rest
        | Quote cell -> cell :: evalRest xs
        
        | Function fn -> evalFn fn xs

let apply services fnName fn args env =
    let newEnv = ExecEnv.makeChild env

    let evalArgAndAddToEnv env (arg, paramName) =
        // eval the arg, then add it to the execution env as the parameter name
        let evaldArg = eval services <| newExpr [arg] env |> Cell.fromList
        ExecEnv.addOrUpdate paramName evaldArg newEnv

    Function.forceParamNames fn
    |> List.zip args
    |> List.iter (evalArgAndAddToEnv env)

    match eval services <| newExpr fn.body newEnv |> List.tryLast with
    | Some result -> Success result
    | None -> Success (Lispt [])