module Flisp.Executable

open System
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

let mapList = defaultExpr [
    Symbol "map"
    (lambda 
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

let mapListAndReturnNum = defaultExpr [
    Lispt [
        Symbol "map"
        (lambda 
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
    Number 5.0
]

[<EntryPoint>]
let main argv =
    let program = mapListAndReturnNum

    let res = eval program

    printfn "%A" res

    0 // return an integer exit code