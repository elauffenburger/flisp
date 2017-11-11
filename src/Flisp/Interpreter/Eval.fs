module Flisp.Interpreter.Eval

open Flisp.Syntax.Common

let handleProcResult result =
    match result with
    | Error err -> failwith err
    | Success cell -> cell

let rec eval expr =
    let resolveSymbol sym success =
        match expr.env.TryGetValue(sym) with
        | (true, cell) -> success cell
        | _ -> sprintf "Symbol %A not found" sym |> Error |> handleProcResult |> List.singleton

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
        // For actual procedures we want to evaluate all inner expressions
        | Procedure proc -> 
            let innerExpression = newExpr xs expr.env |> eval
            let res = proc.Invoke(innerExpression, expr.env) |> handleProcResult

            [res]

        // For metaprocedures, we want to examine the ast directly
        | SpecialForm proc -> [proc.Invoke(xs, expr.env) |> handleProcResult]

        | Symbol sym -> resolveSymbol sym (fun cell -> eval ({ cells = cell::xs; env = expr.env }))
        | Lispt cells -> 
            // We need to evaluate the expression in the list, but only for side effects
            (evalRest cells) |> ignore

            // The last expression is the one that actually gets returned
            evalRest xs 
        | Number _ -> x :: evalRest xs
        | Lambda _ -> x :: evalRest xs 
        | Quote cell -> cell :: evalRest xs