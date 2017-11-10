module Flisp
open System.Collections.Generic
open System
open System.Xml
open System.Security.Policy

type Cell = 
    | Symbol of string
    | Value of obj
    | Number of float
    | Lispt of Cell list
    | Lambda of Lambda
    | Begin
    | End
    | Whitespace
    | Procedure of Proc
and Lambda = { parms: Cell list; body: Cell list }
and ExecEnv = Dictionary<string, Cell>
and ProcResult = Success of Cell | Error of string
and Proc = delegate of Cell list * ExecEnv -> ProcResult

type Expression = {cells: Cell list; env: ExecEnv }

let nil = ()

let newExpr cells env = { cells = cells; env = env }

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
        | Lispt cells -> newExpr cells expr.env |> eval

    | x::xs -> 
        match x with
        | Procedure proc -> 
            let innerExpression = newExpr xs expr.env |> eval
            let res = proc.Invoke(innerExpression, expr.env) |> handleProcResult

            [res]

        | Symbol sym -> resolveSymbol sym (fun cell -> eval ({ cells = cell::xs; env = expr.env }))
        | Number _ -> x :: (newExpr xs expr.env |> eval)
        | Lispt cells -> newExpr cells expr.env |> eval        

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

let proc_map cells (env: ExecEnv) =
    match cells with
    | [Symbol "lambda"; Lispt [ Symbol el; Lispt fn ]; Lispt items] ->
        // Map over items
        let result = items |> List.map (fun item ->
            let newEnv = new Dictionary<string, Cell>(env)

            // Add parameter to environment
            newEnv.[el] = Symbol el

            // eval fn 
            let res = newExpr fn newEnv |> eval

            match res with
            | [x] -> x
            | [] -> Symbol "nil"
            | _ -> Lispt res
        )
        
        Success (Lispt result)
    | _ -> Error "incorrect signature for map"

let makeDefaultEnv() =
    let env = dict [
        "nil", Value nil;
        "map", Procedure (new Proc(proc_map));
        "print", Procedure (new Proc(proc_print));
        "+", Procedure (new Proc(proc_add))
    ]

    new ExecEnv(env)

let defaultExpr cells = newExpr cells <| makeDefaultEnv()

let printNum = defaultExpr [
    Symbol "print"
    Number 3.14
]

let addAndPrintNums = defaultExpr [
    Symbol "print"
    Lispt [
        Symbol "+"
        Number 3.0
        Number 5.0
    ]
]

let mapList = defaultExpr [
    Symbol "map"
    Lispt [
        Symbol "lambda"
        Lispt [
            Symbol "n"
        ]
        Lispt [
            Lispt [
                Symbol "print"
                Symbol "n"
            ]
            Symbol "n"
        ]
    ]
    Lispt [
        Number 3.0
        Number 5.0
        Number 12.0
    ]
]

[<EntryPoint>]
let main argv =
    let program = addAndPrintNums

    let res = eval program

    printfn "%A" res

    Console.ReadKey() |> ignore

    0 // return an integer exit code