module Flisp.Executable

open Syntax.Common
open Flisp.Interpreter.Eval

[<EntryPoint>]
let main argv =
    let program = JankyTests.mapAndDefine

    let res = eval program

    printfn "%A" res

    0 // return an integer exit code