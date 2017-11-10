module Flisp

open System
open System.Collections.Generic
open Flisp.Core.Syntax.Common
open Flisp.Core.Interpreter.Eval

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

let lambda parms body = { parms = parms; body = body }

let mapList = defaultExpr [
    Symbol "map"
    Lambda (lambda 
        [Symbol "n"] 
        [Lispt [
            Symbol "print" 
            Symbol "n"
        ];
        Symbol "n"
    ])
    Quote (Lispt [
        Number 3.0
        Number 5.0
        Number 12.0
    ])
]

[<EntryPoint>]
let main argv =
    let program = mapList

    let res = eval program

    printfn "%A" res

    Console.ReadKey() |> ignore

    0 // return an integer exit code