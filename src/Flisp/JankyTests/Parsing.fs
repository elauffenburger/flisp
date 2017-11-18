module Flisp.JankyTests.Parsing

open Flisp.JankyTests.Models
open Flisp.Syntax.Common

let basic = "(print 'hello)"

let callFunc = "(print (+ 1 2))"

let complex = "(progn (define foo (lambda (n) (+ n 12))) (print (+ 12 foo)) (map 'list (lambda (n) (print n)) '(1 2 3)))"

type ParseTest = { name: string; test: string; logged: string list; results: Cell list option }

let safeParse services str = 
    try 
        services.parse services str |> Some
    with
    | e -> 
        services.log e.Message
        None

let toTest test : TestCase =
    { name = test.name; test = (fun services -> safeParse services test.test |> List.singleton |> collectTestResults); logged = test.logged; results = test.results }

let parseTests =
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
]
|> List.map toTest