module Flisp.Interpreter.Parser

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open Flisp.Syntax.Common

type ParseState =
    | InExpression
    | InString

let private newParseStack() = new Stack<ParseState>()

let private splitAndTrim delim (str: string) = 
    str.Split <| Array.init 1 (fun _ -> delim) 
    |> Array.map (fun str -> str.Trim()) 
    |> Array.toList

let private strToList (str: string) = str.ToCharArray() |> Array.toList
let private charsToString chars = new string(List.toArray chars)
let private pop (stack: Stack<'a>) = match stack.Count with
    | 0 -> None
    | _ -> Some <| stack.Pop()

let private toCell str = match Double.TryParse str with
    | (true, num) -> Number num
    | _ -> Symbol str

let private splitAtIndexWhere pred chars = match List.tryFindIndex pred chars with
    | Some index -> List.splitAt index chars
    | None -> (chars, [])

let private takeSecond (_, b) = b

let parse services (str: string) : Cell = 
    let rec parseInner (str: string) : Cell list = 
        let rest = strToList str 

        match rest with 
        | [] -> []
        | x::xs -> match x with 
            | ' ' -> parseInner <| charsToString xs
            | '(' -> 
                let rec getInnerExpr expr acc stack =
                    let (accLen, stackLen) = (List.length acc, List.length stack)
                    match (accLen = 0, stackLen = 0) with
                    | (true, _) | (_, false) -> 
                        match expr with
                        |  x::xs -> match x with
                            | '(' -> getInnerExpr xs (acc@[x]) (stack@[0])
                            | ')' -> 
                                List.splitAt 1 stack
                                |> takeSecond  
                                |> getInnerExpr xs (acc@[x])

                            | _ -> getInnerExpr xs (acc@[x]) stack
                    | (_, true) -> (acc, expr)

                let (innerExprWithParens, right) = getInnerExpr rest [] [] 
                let innerExpr = 
                    innerExprWithParens
                    |> List.skip 1 
                    |> (List.take <| (List.length innerExprWithParens) - 2)
                    |> charsToString 

                match (parseInner innerExpr |> Lispt, parseInner (charsToString right)) with
                | (first, []) -> [first]
                | (first, second) -> first::second

            | ')' -> failwith "unmatched ')' in expression"
            | '\'' -> (parseInner <| charsToString xs) |> Cell.fromList |> Quote |> List.singleton 
            | _ -> 
                let (symbol, rest) = (splitAtIndexWhere (fun c -> c = ')' || c = '(' || c = ' ') rest)
                let symbolStr = charsToString symbol 

                let res = match Double.TryParse symbolStr with
                    | (true, num) -> Number num
                    | _ -> Symbol symbolStr

                res :: (parseInner <| charsToString rest)

    parseInner str |> Cell.fromList 