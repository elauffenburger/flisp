module Flisp.Interpreter.Eval

open Flisp.Syntax.Common
open Flisp.Syntax

let lambda parms body = Lambda({ parms = parms; body = body })
let newExpr cells env = { cells = cells; env = ExecEnv.makeChild env }

let handleProcResult result =
    match result with
    | Error err -> failwith err
    | Success cell -> cell

let rec eval expr =
    let resolveSymbol sym success =
        match ExecEnv.resolveSymbol sym expr.env with
        | Some cell -> success cell
        | None -> sprintf "Symbol %A not found" sym |> Error |> handleProcResult |> List.singleton

    match expr.cells with
    | [x] ->
        match x with
        | Symbol sym -> resolveSymbol sym (fun cell -> [cell])
        | Lispt cells -> newExpr cells expr.env |> eval
        | Number _ -> [x]
        | Lambda _ -> [x]
        | Quote cell -> [cell]

    | x::xs -> 
        let evalRest rest = newExpr rest expr.env |> eval

        match x with

        // Evaluate all inner expressions and then invoke
        | Procedure proc -> 
            let innerExpression = newExpr xs expr.env |> eval
            proc.Invoke(innerExpression, expr.env) |> handleProcResult |> List.singleton

        // Examine the ast directly without evaluating inner expressions, then invoke 
        | MetaProcedure proc -> proc.Invoke(xs, expr.env) |> handleProcResult |> List.singleton

        // Try to resolve the symbol and then reevaluate with the resolved result
        | Symbol sym -> resolveSymbol sym (fun cell -> eval ({ cells = cell::xs; env = expr.env }))

        // Evaluate the list, but throw away the list eval result and treat last expression as the real result
        | Lispt cells -> 
            (evalRest cells) |> ignore
            evalRest xs 

        // Just hand back the value and evaluate the rest
        | Number _ -> x :: evalRest xs

        // Treat lambdas like any other value
        | Lambda _ -> x :: evalRest xs 

        // Unwrap the quoted contents and evaluate the rest
        | Quote cell -> cell :: evalRest xs