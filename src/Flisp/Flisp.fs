module Flisp.Executable

open Syntax.Common
open JankyTests
open Flisp.Interpreter.Eval
open Flisp.Interpreter.Procedures

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

    let exec test =
        // Converts a Cell option list to a Cell list option
        let collectResults results =
            List.fold (fun acc res -> 
                match (acc, res) with 
                | (Some a, Some r) -> Some (a @ [r])
                | _ -> None
            ) (Some []) results

        // Keeps track of logged messages
        let mutable logged = List.init 0 (fun i -> "")
        let log msg = logged <- logged @ [msg]
        
        // Test services to inject into the interpretor
        let testServices = { log = log; eval = eval; apply = apply }

        // Helper for evaluation: if an eval throws, convert the result to None
        let evalExpr expr env = 
            try 
                eval testServices expr env
                |> Some
            with
            | e -> 
                log e.Message |> ignore
                None

        // Create a shared test environment and run each expression
        let env = makeDefaultEnv()
        let results = List.map (fun expr -> evalExpr expr env) test.test

        printfn "Test: %A\nLogged: %A\nResult: %A\n" test.name logged results

        let testResults = {
            name = test.name
            expectedOutput = test.logged
            actualOutput = logged
            expectedResults = test.results
            actualResults = collectResults results
        }

        // Make sure our console output matches what we expect
        let outputEqual = (List.compareWith (fun (i:string) (j:string) -> i.CompareTo(j)) test.logged logged) = 0

        match outputEqual with
        | true -> passed <- passed @ [testResults]
        | false -> failed <- failed @ [testResults]

    // Run all tests
    List.iter exec allTests

    // Report status
    printfn "passed:\n%A\nfailed:%A\n" (getTestNames passed) (getTestNames failed)

    0