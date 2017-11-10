module Flisp.Core.Syntax.Common
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
    | MetaProcedure of Proc
    | Quote of Cell
and Lambda = { parms: Cell list; body: Cell list }
and ExecEnv = Dictionary<string, Cell>
and ProcResult = Success of Cell | Error of string
and Proc = delegate of Cell list * ExecEnv -> ProcResult

type Expression = {cells: Cell list; env: ExecEnv }

let nil = ()
let lambda parms body = Lambda({ parms = parms; body = body })
let newExpr cells env = { cells = cells; env = env }