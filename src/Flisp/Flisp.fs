module Flisp.Executable

open Syntax.Common
open Flisp.Interpreter.Eval
open Flisp.Interpreter.Procedures
open Flisp.Interpreter.Parser
open Flisp.Interpreter.Exec
open System

let test() = JankyTests.Core.executeAllTests()
let repl() =
    let services = { parse = parse; exec = exec; apply = apply; eval = eval; log = printf "%A"; logRaw = printf "%O" }
    let env = makeDefaultEnv()

    let splitNFromEnd n input = 
        let count = String.length input
        let join (chars: char[]) = String.Join("", chars)

        input.ToCharArray() |> Array.splitAt (count - n) |> (fun (a, b) -> (join a, join b))

    let mutable input = "" 
    let rec read() = 
        services.logRaw <| "\n>"

        input <- input + Console.ReadLine() + "\n"

        match splitNFromEnd 3 input with
        | (expr, ";;\n") -> 
            try
                sprintf "\n%A" <| services.exec services env expr
                |> services.log
                |> ignore
            with
            | e -> services.log <| sprintf "Error in expression: %A" e.Message

            input <- ""
        | _ -> ignore()

        read()
    
    services.logRaw <| "Welcome to Flisp!\n"
    read()

[<EntryPoint>]
let main argv =
    match argv with
    | [||] -> repl()
    | [|"--test"|] -> test()

    0