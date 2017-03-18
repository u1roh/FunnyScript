module FunnyScript.Parser
open System
open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error
open AST

type Parser = Parser<Expr, unit>

let pLiteralNumber : Parser =
  let numfmt =
    NumberLiteralOptions.AllowMinusSign |||
    NumberLiteralOptions.AllowPlusSign |||
    NumberLiteralOptions.AllowFraction |||
    NumberLiteralOptions.AllowExponent
  numberLiteral numfmt "number"
  |>> (fun x -> (if x.IsInteger then Int (int32 x.String) else Float (float x.String)) |> Obj)

let pLiteralString : Parser =
  between (pstring "\"") (pstring "\"") (manyChars (noneOf "\"")) |>> (String >> Obj)

let pIdentifier =
  regex "[a-zA-Z_][0-9a-zA-Z_]*"

let pAtom =
  choice [
    skipString "true"  |>> (fun () -> Obj True)
    skipString "false" |>> (fun () -> Obj False)
    pLiteralNumber
    pLiteralString
    pIdentifier |>> Ref
  ]


let pExpr =
  let pExpr, pExprRef = createParserForwardedToRef<Expr, unit>()
  let str_ws s = skipString s >>. spaces
  let char_ws c = skipChar c >>. spaces
  let opp = new OperatorPrecedenceParser<Expr,unit,unit>()
  opp.TermParser <-
    let pTerm = (pAtom <|> between (str_ws "(") (str_ws ")") pExpr) .>> spaces
    let apply terms = (List.head terms, List.tail terms) ||> List.fold (fun f arg -> Apply (f, arg))
    many1 pTerm |>> apply
  opp.AddOperator(InfixOperator("|>", spaces, 1, Associativity.Left, fun arg f -> Apply (f, arg)))
  opp.AddOperator(InfixOperator("+", spaces, 2, Associativity.Left, fun x y -> BinaryOp (Plus,  x, y)))
  opp.AddOperator(InfixOperator("-", spaces, 2, Associativity.Left, fun x y -> BinaryOp (Minus, x, y)))
  opp.AddOperator(InfixOperator("*", spaces, 3, Associativity.Left, fun x y -> BinaryOp (Mul,   x, y)))
  opp.AddOperator(InfixOperator("/", spaces, 3, Associativity.Left, fun x y -> BinaryOp (Div,   x, y)))
  opp.AddOperator(TernaryOperator("?", spaces, ":", spaces, 1, Associativity.None, fun cond thenExpr elseExpr -> If (cond, thenExpr, elseExpr)))
  let pLet =
    str_ws "let" >>. pIdentifier .>> spaces .>> char_ws '=' .>>. pExpr .>> char_ws ';' .>>. pExpr 
    |>> (fun ((name, expr1), expr2) -> Let (name, expr1, expr2))
  let pLambda =
    skipChar '\\' >>. pIdentifier .>> spaces .>> str_ws "->" .>>. pExpr
    |>> (fun (arg, body) -> FuncDef { Arg = arg; Body = body })
  pExprRef :=
    choice [
      pLet
      pLambda
      opp.ExpressionParser
    ]
  pExpr

let parse program =
  match run pExpr program with
  | Success (x, _, _)   -> Some x
  | Failure (msg, _, _) -> None

let test() =
  parse "1" |> printfn "%A"
  parse "3.14" |> printfn "%A"
  parse "\"hello, FParsec. \"" |> printfn "%A"
  parse "hoge123piyo" |> printfn "%A"

  let expr = parse "1 + 2*(3 + 4)"
  printfn "%A" expr
