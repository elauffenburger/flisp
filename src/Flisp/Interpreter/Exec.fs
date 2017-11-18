module Flisp.Interpreter.Exec

open Flisp.Syntax.Common
open Flisp.Interpreter.Procedures

let exec services env str = 
    let expr = services.parse services str 
    services.eval services expr env

    