module Flisp.Syntax.Common
open System.Collections.Generic

type Cell = 
    | Symbol of string
    | Value of obj
    | Number of float
    | Lispt of Cell list
    | Function of Function
    | Procedure of Proc
    | MetaProcedure of Proc
    | Quote of Cell
and Function = { parms: Cell list; body: Cell list; env: ExecEnv }
and ExecEnv = { data: Dictionary<string, Cell>; parent: ExecEnv option }
and ProcResult = Success of Cell | Error of string
and Services = { log: (string -> unit) }
and Proc = Services -> Cell list -> ExecEnv -> ProcResult

type Expression = {cells: Cell list; env: ExecEnv }

let nil = ()

module Cell = 
    let fromList cells = match cells with
        | [x] -> x
        | xs -> Lispt xs

module Symbol =
    type GetNameResult = Success of string | Error

    let getName cell = match cell with
    | Symbol sym -> Success sym
    | _ -> Error

    let isSuccess res = match res with
    | Success _ -> true
    | Error -> false

module ExecEnv =
    let addOrUpdate key value env = env.data.[key] <- value 

    let make (data: IDictionary<string, Cell>) = { data = new Dictionary<string, Cell>(data); parent = None}
    let makeChild env = { data = new Dictionary<string, Cell>(env.data); parent = Some env }

    let rec resolveSymbol symbol env = 
        match env.data.TryGetValue(symbol) with
        | (true, cell) -> Some cell
        | _ -> match env.parent with
            | Some parent -> resolveSymbol symbol parent
            | None -> None

    let parentOrSelf env = 
        match env.parent with
        | Some parent -> parent
        | None -> env

module Function =
    let paramNames fn = List.map Symbol.getName fn.parms
    let forceParamNames fn = 
        paramNames fn 
        |> List.map (fun nameRes -> 
            match nameRes with
            | Symbol.GetNameResult.Success name -> name
            | Symbol.GetNameResult.Error -> failwith ""
        )
    
    type ValidationResult = 
        | Success of string * Function * Cell list * ExecEnv
        | Error of string

    let (>>=) res validateFn = 
        match res with
        | Success (fnName, fn, args, env) -> validateFn fnName fn args env
        | Error _ -> res

    let validateInvocation fnName fn args env =
        let paramsLength = List.length fn.parms
        let argsLength = List.length args

        if paramsLength = argsLength
        then Success (fnName, fn, args, env)
        else Error <| sprintf "%A expected %A params and received %A args" fnName paramsLength argsLength

    let validateFunction fnName fn args env =
        if paramNames fn |> List.forall Symbol.isSuccess
        then Success (fnName, fn, args, env)
        else Error "Some parameters for the Function somehow resolved to non-Symbol values"

    let validateSymbol fnName _ args env =
        match ExecEnv.resolveSymbol fnName env with
        | Some (Function fn) -> Success (fnName, fn, args, env)
        | Some sym -> Error <| sprintf "First arg to funcall should resolve to Function, but resolved to %A" sym
        | None _ -> Error <| sprintf "Could not resolve Symbol %A" fnName
    
    let validate fnName args env =
        validateSymbol fnName None args env
        >>= validateSymbol
        >>= validateFunction 
        >>= validateInvocation