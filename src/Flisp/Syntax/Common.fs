module Flisp.Syntax.Common
open System.Collections.Generic

type Cell = 
    | Symbol of string
    | Value of obj
    | Number of float
    | Lispt of Cell list
    | Lambda of Lambda
    | Begin
    | End
    | Whitespace
    | Procedure of Proc
    | SpecialForm of Proc
    | Quote of Cell
and Lambda = { parms: Cell list; body: Cell list }
and ExecEnv = { data: Dictionary<string, Cell>; parent: ExecEnv option }
and ProcResult = Success of Cell | Error of string
and Proc = delegate of Cell list * ExecEnv -> ProcResult

type Expression = {cells: Cell list; env: ExecEnv }

let nil = ()