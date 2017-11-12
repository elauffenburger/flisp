module Flisp.JankyTests

open Syntax.Common
open Flisp.Interpreter.Procedures
open Flisp.Interpreter.Eval

let printNum = defaultExpr [
    Symbol "print"
    Number 3.14
]

let addAndPrintNums = defaultExpr [
    Symbol "print"
    Lispt [
        Symbol "+"
        Number 3.0
        Number 5.0
    ]
]

let mapList = defaultExpr [
    Symbol "map"
    (lambda 
        [Symbol "n"] 
        [
            Lispt [Symbol "print"; Symbol "n"]
            Lispt [Symbol "+"; Symbol "n"; Number 1.0]
        ])
    Quote (Lispt [
        Number 3.0
        Number 5.0
        Number 12.0
    ])
]

let mapListAndReturnNum = defaultExpr [
    Lispt [
        Symbol "map"
        (lambda 
            [Symbol "n"] 
            [Lispt [
                Symbol "print" 
                Symbol "n"
            ];
            Symbol "n"
        ])
        Quote (Lispt [
            Number 3.0
            Number 5.0
            Number 12.0
        ])
    ]
    Number 5.0
]

let mapNestedList = defaultExpr [
    Symbol "map"
    (lambda 
        [Symbol "n"]
        [Lispt [
            Symbol "map"
            (lambda 
                [Symbol "n"]
                [Lispt [
                    Symbol "+"
                    Symbol "n"
                    Number 1.0
                ]])
            Symbol "n"
        ]])
    Quote (Lispt
        [
        Lispt [
            Number 1.0
            Number 2.0
            Number 3.0
        ]
        Lispt [
            Number 4.0
            Number 5.0
            Number 6.0
        ]
        Lispt [
            Number 7.0
            Number 8.0
            Number 9.0
        ]
    ])
]

let defineAndPrint = defaultExpr [
    Lispt [
        Symbol "define"
        Symbol "foo"
        Number 42.0
    ]
    Lispt [
        Symbol "print"
        Symbol "foo"
    ]
]

let defineDoesNotClobber = defaultExpr [
    Lispt [
        Symbol "define"
        Symbol "foo"
        Number 42.0
    ]
    Lispt [
        Lispt [
            Symbol "define"
            Symbol "foo"
            Number 50.0
        ]
        Lispt [
            Symbol "print"
            Symbol "foo"
        ]
    ]
    Lispt [
        Symbol "print"
        Symbol "foo"
    ]
]

let mapAndDefine = defaultExpr [
    Lispt [
        Symbol "map"
        (lambda
            [Symbol "n"]
            [Lispt [
                Lispt[Symbol "define"; Symbol "foo"; Lispt [Symbol "+"; Symbol "n"; Number 1.0]]
                Lispt[Symbol "print"; Symbol "foo"]
            ]]
        )
        Lispt [Number 1.0; Number 2.0] |> Quote
    ]
]

type TestCase = { name: string; test: Expression; logged: string list; result: Cell list }

let allTests = [
    { 
        name = "printNum" 
        test = printNum 
        logged = ["[Number 3.14]"]
        result = [Symbol "nil"]
    }
    { 
        name = "addAndPrintNums" 
        test = addAndPrintNums
        logged = ["[Number 8.0]"]
        result = [Symbol "nil"]
    }
    { 
        name = "mapList" 
        test = mapList
        logged = ["[Number 3.0]"; "[Number 5.0]"; "[Number 12.0]"]
        result = [Lispt [Number 4.0; Number 6.0; Number 13.0]]     
    }
    { 
        name = "mapListAndReturnNum" 
        test = mapListAndReturnNum
        logged = ["[Number 3.0]"; "[Number 5.0]"; "[Number 12.0]"]
        result = [Number 5.0]
    }
    { 
        name = "mapNestedList" 
        test = mapNestedList
        logged = []
        result = [Lispt [
            Lispt [Number 2.0; Number 3.0; Number 4.0]
            Lispt [Number 5.0; Number 6.0; Number 8.0]
            Lispt [Number 8.0; Number 9.0; Number 10.0]
        ]]
    }
    { 
        name = "defineAndPrint" 
        test = defineAndPrint
        logged = ["[Number 42.0]"]
        result = [Symbol "nil"]
    }
    { 
        name = "defineDoesNotClobber"
        test = defineDoesNotClobber
        logged = ["[Number 50.0]"; "[Number 42.0]"]
        result = [Symbol "nil"]
    }
    { 
        name = "mapAndDefine"
        test = mapAndDefine
        logged = ["[Number 2.0]"; "[Number 3.0]"]
        result = [Lispt [Symbol "nil"; Symbol "nil"]]
    }
]