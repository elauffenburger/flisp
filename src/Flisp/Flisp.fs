module Flisp.Executable

open Syntax.Common
open JankyTests.Models
open JankyTests.Expressions
open JankyTests.Parsing
open Flisp.Interpreter.Eval
open Flisp.Interpreter.Procedures
open Parser

type TestResult = { 
    name: string
    expectedOutput: string list 
    actualOutput: string list
    expectedResults: Cell list option
    actualResults: Cell list option
 }

[<EntryPoint>]
let main argv =
    let mutable passed: TestResult list = []
    let mutable failed: TestResult list = []

    let getTestNames tests = List.map (fun t -> t.name) tests

    let exec (test: TestCase) =
        // Keeps track of logged messages
        let mutable logged = List.init 0 (fun i -> "")
        let log msg = logged <- logged @ [msg]
        
        // Test services to inject into the interpreter
        let testServices = { log = log; eval = eval; apply = apply; parse = parse }

        let results = test.test testServices

        printfn "Test: %A\nLogged: %A\nResult: %A\n" test.name logged results

        let testResults = {
            name = test.name
            expectedOutput = test.logged
            actualOutput = logged
            expectedResults = test.results
            actualResults = results
        }

        // Make sure our console output matches what we expect
        let outputEqual = (List.compareWith (fun (i:string) (j:string) -> i.CompareTo(j)) test.logged logged) = 0

        match outputEqual with
        | true -> passed <- passed @ [testResults]
        | false -> failed <- failed @ [testResults]

    // Run all tests
    List.iter exec <| expressionTests @ parseTests
    |> ignore

    // Report status
    printfn "passed:\n%A\nfailed:%A\n" (getTestNames passed) (getTestNames failed)

    0