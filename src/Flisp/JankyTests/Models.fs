module Flisp.JankyTests.Models

open Flisp.Syntax.Common

type TestCase = { name: string; test: Services -> Cell list option; logged: string list; results: Cell list option }

// Converts a Cell option list to a Cell list option
let collectTestResults results =
    List.fold (fun acc res -> 
        match (acc, res) with 
        | (Some a, Some r) -> Some (a @ [r])
        | _ -> None
    ) (Some []) results