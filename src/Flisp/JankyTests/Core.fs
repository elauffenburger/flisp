module Flisp.JankyTests.Core

    open Flisp.Syntax.Common
    open Flisp.JankyTests.Models
    open Flisp.JankyTests.Expressions
    open Flisp.JankyTests.Parsing
    open Flisp.Interpreter.Eval
    open Flisp.Interpreter.Procedures
    open Flisp.Interpreter.Parser
    open Flisp.Interpreter.Exec
    open Flisp.JankyTests.Combined

    type TestResult = { 
        name: string
        expectedOutput: string list 
        actualOutput: string list
        expectedResults: Cell list option
        actualResults: Cell list option
    }

    let mutable passed: TestResult list = []
    let mutable failed: TestResult list = []

    let getTestNames tests = List.map (fun t -> t.name) tests

    let executeTests (test: TestCase) =
        // Keeps track of logged messages
        let mutable logged = List.init 0 (fun i -> "")
        let log msg = logged <- logged @ [msg]
        
        // Test services to inject into the interpreter
        let testServices = { log = log; logRaw = log; eval = eval; apply = apply; parse = parse; exec = (fun services env str -> exec services env str) }

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

    let executeAllTests() = 
        // Run all tests
        expressionTests @ parseTests @ combinedTests
        |> List.iter executeTests
        |> ignore

        // Report status
        printfn "passed:\n%A\nfailed:%A\n" (getTestNames passed) (getTestNames failed)
