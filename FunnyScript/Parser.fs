module FunnyScript.Parser
open System
open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

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
  between (pstring "\"") (pstring "\"") (manyChars (noneOf "\"")) |>> (Str >> Obj)

let pIdentifier =
  regex "[a-zA-Z_][0-9a-zA-Z_]*"

let pAtom =
  choice [
    skipString "true"  |>> (fun () -> Obj True)
    skipString "false" |>> (fun () -> Obj False)
    pLiteralNumber
    pLiteralString
    pIdentifier |>> Ref
  ] .>> spaces


let pExpr =
  let pExpr, pExprRef = createParserForwardedToRef<Expr, unit>()
  let str_ws  s = skipString s >>. spaces
  let char_ws c = skipChar   c >>. spaces

//  let pLetPhrase =
//    str_ws "let" >>. pIdentifier .>> spaces .>> char_ws '=' .>>. pExpr .>> char_ws ';'
  let pLetPhrase =
    pIdentifier .>> spaces .>> str_ws ":=" .>>. pExpr .>> char_ws ';'
  let pLet =
    pLetPhrase .>>. pExpr
    |>> (fun ((name, expr1), expr2) -> Let (name, expr1, expr2))
  let pDo =
    str_ws "do" >>. pExpr .>> char_ws ';' .>>. opt pExpr
    |>> (fun (expr1, expr2) -> match expr2 with Some expr2 -> Combine (expr1, expr2) | _ -> expr1)
  let pLambda =
    pIdentifier .>> spaces .>> str_ws "->" .>>. pExpr
    |>> (fun (arg, body) -> FuncDef { Arg = arg; Body = body })
  let pRecord =
    between (str_ws "{") (str_ws "}") (many pLetPhrase)
    |>> NewRecord
  let pList =
    between (str_ws "[") (str_ws "]") (sepBy pExpr (char_ws ','))
    |>> (List.toArray >> NewList)

  let opp = new OperatorPrecedenceParser<Expr,unit,unit>()
  opp.TermParser <-
    let pTerm =
      choice [ pAtom; pList; pRecord; between (str_ws "(") (str_ws ")") pExpr ]
      .>>. opt (char_ws '.' >>. pIdentifier .>> spaces)
      |>> function
        | (expr, None) -> expr
        | (expr, Some name) -> RefMember (expr, name)
    let apply terms = (List.head terms, List.tail terms) ||> List.fold (fun f arg -> Apply (f, arg))
    many1 pTerm |>> apply

  let infixOp  str precedence mapping = InfixOperator(str, spaces, precedence, Associativity.Left, mapping)
  let binaryOp str precedence op      = infixOp str precedence (fun x y -> BinaryOp (op, x, y))
  [ binaryOp "*"  9 Mul
    binaryOp "/"  9 Div
    binaryOp "+"  8 Plus
    binaryOp "-"  8 Minus
    binaryOp "::" 7 Cons
    infixOp  "|>" 5 (fun arg f -> Apply (f, arg))
    binaryOp "<"  4 Less
    binaryOp ">"  4 Greater
    binaryOp "<=" 4 LessEq
    binaryOp ">=" 4 GreaterEq
    binaryOp "==" 3 Equal
    binaryOp "!=" 3 NotEq
    binaryOp ":?" 3 Is
  ] |> List.iter opp.AddOperator
  opp.AddOperator(TernaryOperator("?", spaces, ":", spaces, 2, Associativity.None, fun cond thenExpr elseExpr -> If (cond, thenExpr, elseExpr)))
  pExprRef :=
    choice [
      pDo
      attempt pLet
      attempt pLambda
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
