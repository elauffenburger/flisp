module Flisp.Interpreter.Exec

open Flisp.Syntax.Common
open Flisp.Interpreter.Procedures

/// <summary>Evaluates a string representation of an exprsesion</summary>
let exec services env str = 
    let expr = services.parse services str 
    services.eval services expr env

    