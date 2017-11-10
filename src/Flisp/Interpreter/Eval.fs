module Flisp.Core.Interpreter.Eval

open System.Collections.Generic
open Flisp.Core.Syntax.Common

let handleProcResult result =
    match result with
    | Error err -> failwith err
    | Success cell -> cell

let rec eval expr =
    let resolveSymbol sym success =
        match expr.env.TryGetValue(sym) with
        | (true, cell) -> success cell
        | _ -> [handleProcResult (Error "Symbol not found")] 

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
        | MetaProcedure proc -> [proc.Invoke(xs, expr.env) |> handleProcResult]

        | Symbol sym -> resolveSymbol sym (fun cell -> eval ({ cells = cell::xs; env = expr.env }))
        | Lispt cells -> 
            // We need to evaluate the expression in the list, but only for side effects
            (evalRest cells) |> ignore

            // The last expression is the one that actually gets returned
            evalRest xs 
        | Number _ -> x :: evalRest xs
        | Lambda _ -> x :: evalRest xs 
        | Quote cell -> cell :: evalRest xs

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

let map cells (env: ExecEnv) =
    match cells with

    // We're looking for a very specific signature for map
    | [Lambda ({ parms = [Symbol el]; body = fn}); Lispt items] ->
        // The functor
        let iterMap item = 
            let newEnv = new Dictionary<string, Cell>(env)

            // Add functor parameter to environment
            newEnv.Add(el, item)

            // eval fn 
            let res = newExpr fn newEnv |> eval

            match res with
            | [x] -> x
            | [] -> Symbol "nil"
            | _ -> Lispt res

        // Map over items
        let result = items |> List.map iterMap
        
        Success (Lispt result)

    | _ -> Error "incorrect signature for map"

let makeDefaultEnv() =
    let env = dict [
        "nil", Value nil;
        "map", Procedure (new Proc(map));
        "print", Procedure (new Proc(print));
        "+", Procedure (new Proc(add))
    ]

    new ExecEnv(env)

let defaultExpr cells = newExpr cells <| makeDefaultEnv()