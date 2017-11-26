module Flisp.JankyTests.Combined

open Flisp.JankyTests.Models
open Flisp.Syntax.Common
open Flisp.Interpreter.Procedures

let basic = "(print 'hello)"

let callFunc = "(print (+ 1 2))"

let complex = "(progn ((define foo 5) (define bar 10) (map (lambda (n) (+ foo (+ bar (+ n 27)))) '(0 0 0))))"

let defun = "(progn ((defun the-fun (n) (print n)) (the-fun 5)))"

type CombinedTest = { name: string; test: string; logged: string list; results: Cell list option }

let safeExec services str = 
    try 
        services.exec services (makeDefaultEnv()) str |> Some
    with
    | e -> 
        services.log e.Message
        None

let toTest test : TestCase =
    { name = test.name; test = (fun services -> safeExec services test.test |> List.singleton |> collectTestResults); logged = test.logged; results = test.results }

let combinedTests =
[
    {
        name = "basic"
        test = basic
        logged = []
        results = Some []
    }
    {
        name = "callFunc"
        test = callFunc
        logged = []
        results = Some []
    }
    {
        name = "complex"
        test = complex
        logged = []
        results = Some []
    }
    {
        name = "defun"
        test = defun
        logged = []
        results = Some []
    }
]
|> List.map toTest