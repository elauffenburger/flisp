module Flisp.Interpreter.Parser

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open Flisp.Syntax.Common
open Flisp.Helpers.String

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

/// <summary>Given a string input, returns a Cell representation that can be evaluated</summary>
let parse services (str: string) : Cell = 
    let rec parseInner (str: string) : Cell list = 
        let rest = strToList str 

        match rest with 
        | [] -> []
        | x::xs -> match x with 
            | ' ' -> parseInner <| charsToString xs
            | '(' -> 
                // Splits the input into the next immediate expression and everything after
                // that expression (meaning, splits "(foo (bar) baz)" into "foo, (bar) baz", parses "foo",
                // then parses "(bar) baz")

                // Gets an expression surrounded by "()"
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

                // Get the expression inside of "()"
                let innerExpr = 
                    innerExprWithParens
                    |> List.skip 1 
                    |> (List.take <| (List.length innerExprWithParens) - 2)
                    |> charsToString 

                // Parse the inner expression, then parse the rest independently
                // So, this is what parses "foo" and "(bar) baz" independently, then joins the results
                match (parseInner innerExpr |> Lispt, parseInner (charsToString right)) with
                | (first, []) -> [first]
                | (first, second) -> first::second

            | ')' -> failwith "unmatched ')' in expression"
            | '\'' -> (parseInner <| charsToString xs) |> Cell.fromList |> Quote |> List.singleton 
            | _ -> 
                // If we're not dealing with a special character, we're parsing a symbol
                let (symbol, rest) = (splitAtIndexWhere (fun c -> c = ')' || c = '(' || c = ' ') rest)
                let symbolStr = charsToString symbol 

                let res = match Double.TryParse symbolStr with
                    | (true, num) -> Number num
                    | _ -> Symbol symbolStr

                res :: (parseInner <| charsToString rest)
    
    let sanitize (str: string): string =
        replace "\r\n" " " str
        |> replace "\n" " "

    sanitize str
    |> parseInner 
    |> Cell.fromList 