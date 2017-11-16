module Flisp.Syntax.Common
open System.Collections.Generic

type Cell = 
    | Symbol of string
    | Value of obj
    | String of string
    | Number of float
    | Lispt of Cell list
    | Function of Function
    | Procedure of Proc
    | MetaProcedure of Proc
    | Quote of Cell
and Function = { name: string; parms: Cell; body: Cell; env: ExecEnv }
and ExecEnv = { data: Dictionary<string, Cell>; parent: ExecEnv option }
and ProcResult = Success of Cell | Error of string
and Services = { log: (string -> unit); eval: (Services -> Cell -> ExecEnv -> Cell); apply: (Services -> string -> Function -> Cell list -> ExecEnv -> ProcResult) }
and Proc = Services -> Cell list -> ExecEnv -> ProcResult

let nil = Symbol "nil"

module Cell = 
    let fromList cells = match cells with
        | [x] -> x
        | xs -> Lispt xs
    
    let toList cell = match cell with
        | Lispt cells -> cells
        | _ -> [cell]

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
    
    let forceResolveSymbol symbol env = match resolveSymbol symbol env with
    | Some cell -> cell
    | None -> failwithf "Failed to resolve symbol %A" symbol

    let parentOrSelf env = 
        match env.parent with
        | Some parent -> parent
        | None -> env

module Function =
    let paramNames fn = List.map Symbol.getName <| Cell.toList fn.parms
    let forceParamNames fn = 
        paramNames fn 
        |> List.map (fun nameRes -> 
            match nameRes with
            | Symbol.GetNameResult.Success name -> name
            | Symbol.GetNameResult.Error -> failwith "failed to resolve param name"
        )

    let forceInvoke apply cell = match cell with
        | Function fn -> apply fn []
        | _ -> failwithf "Failure trying to invoke %A" cell
    
    let forceInvokeWithArgs services fn args lexEnv = match fn with
        | Procedure proc -> 
            let evaldArgs = List.map (fun x -> services.eval services x lexEnv) args

            proc services evaldArgs lexEnv
        | MetaProcedure proc -> proc services args lexEnv
        | Function fn -> services.apply services fn.name fn args fn.env
        | _ -> failwithf "forceInvokeWithArgs was provided a non-invocable type as its first arg: %A" fn
    
    type ValidationResult = 
        | Success of string * Function * Cell * ExecEnv
        | Error of string

    let (>>=) res validateFn = 
        match res with
        | Success (fnName, fn, args, env) -> validateFn fnName fn args env
        | Error _ -> res

    let validateInvocation fnName fn args env =
        let paramsLength = List.length <| Cell.toList fn.parms
        let argsLength = List.length <| Cell.toList args

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