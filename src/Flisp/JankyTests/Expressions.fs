module Flisp.JankyTests.Expressions

open Flisp.JankyTests.Models
open Flisp.Syntax.Common
open Flisp.Interpreter.Procedures

let printNum = [Lispt [
    Symbol "print"
    Number 3.14
]]

let addAndPrintNums = [Lispt [
    Symbol "print"
    Lispt [
        Symbol "+"
        Number 3.0
        Number 5.0
    ]
]]

let mapList = [Lispt [
    Symbol "map"
    Lispt [
        Symbol "lambda"
        Lispt [Symbol "n"] 
        Lispt [
            Symbol "progn"
            Lispt [
                Lispt [Symbol "print"; Symbol "n"]
                Lispt [Symbol "+"; Symbol "n"; Number 1.0]
            ]
        ]
    ]
    Quote (Lispt [
        Number 3.0
        Number 5.0
        Number 12.0
    ])
]]

let defineAndPrint = [
    Lispt [
        Symbol "define"
        Symbol "foo"
        Number 42.0
    ]
    Lispt [
        Symbol "print"
        Symbol "foo"
    ]
]

let mapAndDefine = [Lispt [
    Symbol "map"
    Lispt [
        Symbol "lambda"
        Lispt [Symbol "n"]
        Lispt [
            Symbol "progn"
            Lispt [
                Lispt [Symbol "define"; Symbol "foo"; Lispt [Symbol "+"; Symbol "n"; Number 1.0]]
                Lispt [Symbol "print"; Symbol "foo"]
            ]
        ]
    ]
    Lispt [Number 1.0; Number 2.0] |> Quote
]]

let funcall = [Lispt [
    Symbol "progn"
    Lispt [
        Lispt [
            Symbol "define"
            Symbol "funlambda"
            Lispt [
                Symbol "lambda"
                Lispt [Symbol "x"; Symbol "y"]
                Lispt [Symbol "print"; Lispt[Symbol "+"; Symbol "x"; Symbol "y"]]
            ]
        ]
        Lispt [
            Symbol "define"
            Symbol "foo"
            Number 2.0
        ]
        Lispt [
            Symbol "funcall"
            Symbol "funlambda"
            Lispt [
                Symbol "foo"
                Number 40.0
            ]
        ]
    ]
]]

type ExpressionTest = { name: string; test: Cell list; logged: string list; results: Cell list option }

// Helper for evaluation: if an eval throws, convert the result to None
let evalExpr services expr env = 
    try 
        Some <| services.eval services expr env
    with
    | e -> 
        services.log e.Message |> ignore
        None

let toTest test = 
    let env = makeDefaultEnv()
    let toTestFn cells = (fun services -> List.map (fun cell -> evalExpr services cell env) cells |> collectTestResults)

    let result: TestCase = { name = test.name; test = toTestFn test.test; logged = test.logged; results = test.results }
    result

let expressionTests = 
    [
        { 
            name = "printNum" 
            test = printNum 
            logged = ["Number 3.14"]
            results = Some [Symbol "nil"]
        }
        { 
            name = "addAndPrintNums" 
            test = addAndPrintNums
            logged = ["Number 8.0"]
            results = Some [Symbol "nil"]
        }
        { 
            name = "mapList" 
            test = mapList
            logged = ["Number 3.0"; "Number 5.0"; "Number 12.0"]
            results = Some [Lispt [Number 4.0; Number 6.0; Number 13.0]]
        }
        { 
            name = "defineAndPrint" 
            test = defineAndPrint
            logged = ["Number 42.0"]
            results = Some [Symbol "nil"]
        }
        { 
            name = "mapAndDefine"
            test = mapAndDefine
            logged = ["Number 2.0"; "Number 3.0"]
            results = Some [Lispt [Symbol "nil"; Symbol "nil"]]
        }
        { 
            name = "funcall"
            test = funcall
            logged = ["Number 42.0"]
            results = Some [Symbol "nil"]
        }
    ] |> List.map toTest 