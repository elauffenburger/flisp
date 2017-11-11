module Flisp.Syntax.ExecEnv

open System.Collections.Generic
open Flisp.Syntax.Common

let addOrUpdate key value (env: ExecEnv) =
    env.Remove(key) |> ignore
    env.Add(key, value)

let clone (env: ExecEnv) = new Dictionary<string, Cell>(env)