module Flisp.Syntax.ExecEnv

open System.Collections.Generic
open Flisp.Syntax.Common

let addOrUpdate key value env = env.data.[key] <- value 

let make (data: IDictionary<string, Cell>) = { data = new Dictionary<string, Cell>(data); parent = None}
let makeChild env = { data = new Dictionary<string, Cell>(env.data); parent = Some env }
let resolveSymbol symbol env = 
    match env.data.TryGetValue(symbol) with
    | (true, cell) -> Some cell
    | _ -> None

let parentOrSelf env = 
    match env.parent with
    | Some parent -> parent
    | None -> env