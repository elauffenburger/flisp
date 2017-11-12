module Flisp.Executable

open Syntax.Common
open Flisp.JankyTests
open Flisp.Core.Logger
open Flisp.Interpreter.Eval
open System.Runtime.InteropServices

type TestResult = { 
    name: string
    expectedOutput: string list 
    actualOutput: string list
    expectedResult: Cell list
    actualResult: Cell list 
 }

[<EntryPoint>]
let main argv =
    let mutable passed: TestResult list = []
    let mutable failed: TestResult list = []

    let exec test =
        let mutable logged = List.init 0 (fun i -> "")
        let logger msg = 
            logged <- logged @ [msg]
        
        let testServices = { log = logger }

        let result = eval testServices test.test
        printfn "Test: %A\nLogged: %A\nResult: %A\n" test.name logged result

        let testResult = {
            name = test.name
            expectedOutput = test.logged
            actualOutput = logged
            expectedResult = test.result
            actualResult = result
        }

        let outputEqual = (List.compareWith (fun (i:string) (j:string) -> i.CompareTo(j)) test.logged logged) = 0
        //let outputEqual = (List.compareWith (fun (i) (j) -> i = j) test.result result) = 0

        match outputEqual with
        | true -> passed <- passed @ [testResult]
        | false -> failed <- failed @ [testResult]

    List.iter exec JankyTests.allTests

    let testNames tests = List.map (fun t -> t.name) tests
    printfn "passed:\n%A\nfailed:%A\n" (testNames passed) (testNames failed)

    0 // return an integer exit code