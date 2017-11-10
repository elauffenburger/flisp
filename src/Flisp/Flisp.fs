module Flisp
open System.Collections.Generic
open System
open System.Xml

type Cell = 
    | Symbol of string
    | Value of obj
    | Number of float
    | List of Cell list
    | Begin
    | End
    | Whitespace
    | Procedure of Proc

and ExecEnv = IDictionary<string, Cell>
and ProcResult = Success of Cell | Error of string
and Proc = delegate of Cell list * ExecEnv -> ProcResult

type Expression = {cells: Cell list; env: ExecEnv }

let nil = ()

let proc_print cells env =
    match cells with
    | [] -> Success (Symbol "nil")
    | _ -> 
        printfn "%A" cells 
        Success (Symbol "nil")

let proc_add cells env =
    match cells with 
    | [Number x; Number y] -> Success (Number (x+y))
    | _ -> Error "something bad"

let makeDefaultEnv() =
    dict [
        "nil", Value nil;
        "print", Procedure (new Proc(proc_print));
        "+", Procedure (new Proc(proc_add))
    ]

let newExpr cells env = { cells = cells; env = env }
let defaultExpr cells = newExpr cells <| makeDefaultEnv()

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
        | Number _ -> [x]
        | List cells -> newExpr cells expr.env |> eval

    | x::xs -> 
        match x with
        | Procedure proc -> 
            let innerExpression = newExpr xs expr.env |> eval
            let res = proc.Invoke(innerExpression, expr.env) |> handleProcResult

            [res]

        | Symbol sym -> resolveSymbol sym (fun cell -> eval ({ cells = cell::xs; env = expr.env }))
        | Number _ -> x :: (newExpr xs expr.env |> eval)
        | List cells -> newExpr cells expr.env |> eval        

let printNum = defaultExpr [
    Symbol "print"
    Number 3.14
]

let addAndPrintNums = defaultExpr [
    Symbol "print"
    List [
        Symbol "+"
        Number 3.0
        Number 5.0
    ]
]

[<EntryPoint>]
let main argv =
    let program = addAndPrintNums

    let res = eval program

    printfn "%A" res

    Console.ReadKey() |> ignore

    0 // return an integer exit code