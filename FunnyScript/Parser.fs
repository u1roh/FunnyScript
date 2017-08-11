module FunnyScript.Parser
open System
open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

//type Result = Result<Expr, string * ParserError>
type Result = Result<Expr, string>
type Parser = Parser<Expr, unit>

let private convPos (pos : Position) =
  { FilePath = pos.StreamName; Line = int pos.Line; Column = int pos.Column }

let private toTracable parser =
  parser .>>. getPosition |>> fun (x, pos) -> { Value = x; Position = Some (convPos pos) }

let pLiteralNumber =
  let numfmt =
    //NumberLiteralOptions.AllowMinusSign |||
    //NumberLiteralOptions.AllowPlusSign |||
    NumberLiteralOptions.AllowFraction |||
    NumberLiteralOptions.AllowExponent
  numberLiteral numfmt "number" .>> spaces
  |>> (fun x -> (if x.IsInteger then Int (int32 x.String) else Float (float x.String)) |> Obj)

let pLiteralString =
  let escapeChars = @"""\nt"
  let escapeChar =
    pchar '\\' >>. anyOf escapeChars
    |>> function '"' -> "\"" | '\\' -> "\\" | 'n' -> "\n" | 't' -> "\t" | _ -> failwith "escape char error"
  let nonEscapeChars =
    pchar '\\' >>. noneOf escapeChars
    |>> fun ch -> "\\" + string ch
  let stringChar =
    choice [
      noneOf "\\\"" |>> string
      escapeChar
      nonEscapeChars
    ]
  many stringChar
  |> between (pchar '"') (pchar '"')
  .>> spaces
  |>> (String.concat "" >> box >> ClrObj >> Obj)

let pIdentifier =
  regex "[a-zA-Z_][0-9a-zA-Z_]*" .>> spaces

let pAtom =
  choice [
    skipString "true"  .>> spaces |>> (fun () -> Obj True)
    skipString "false" .>> spaces |>> (fun () -> Obj False)
    pLiteralNumber
    pLiteralString
    pIdentifier |>> Ref
  ]


let pExpr =
  let pExpr, pExprRef = createParserForwardedToRef<Expr, unit>()
  let str_ws  s = skipString s >>. spaces
  let char_ws c = skipChar   c >>. spaces
  let between_ws c1 c2 p = between (char_ws c1) (char_ws c2) p
  let sepByComma p = sepBy p (char_ws ',')

  let pLet =
    opt pIdentifier .>> str_ws ":=" .>>. pExpr .>> char_ws ';' .>>. pExpr
    |>> function
      | ((Some name, expr1), expr2) -> Let (name, expr1, expr2)
      | ((_, expr1), expr2)         -> Combine (expr1, expr2)
  let pDo =
    str_ws "do" >>. pExpr .>> char_ws ';' .>>. opt pExpr
    |>> (fun (expr1, expr2) -> match expr2 with Some expr2 -> Combine (expr1, expr2) | _ -> Combine (expr1, { Value = Obj Null; Position = None }))
  let pIf =
    (opt (char_ws '|') .>> char_ws '?') >>. pExpr .>> str_ws "=>" .>>. pExpr .>> char_ws '|' .>>. pExpr
    |>> fun ((cond, expr1), expr2) -> If (cond, expr1, expr2)
  let pLambda =
    choice [
      pIdentifier |>> (fun x -> [x])
      between_ws '(' ')' (sepByComma pIdentifier)
    ]
    .>> str_ws "->" .>>. pExpr
    |>> (fun (args, body) -> FuncDef { Args = args; Body = body })
  let pRecord =
    between_ws '{' '}' (many (pIdentifier .>> str_ws ":=" .>>. pExpr .>> char_ws ';'))
    |>> NewRecord
  let pList =
    between_ws '[' ']'
      (choice [
        attempt (pExpr .>> str_ws ".." .>>. pExpr |>> ListByRange)
        sepByComma pExpr |>> (List.toArray >> NewList)
      ])
  let pTuple =
    between_ws '(' ')' (sepByComma pExpr)
    |>> function
      | [] -> Obj Null
      | [expr] -> expr.Value
      | tuple  -> tuple |> List.toArray |> NewList
  let pTermItem =
    choice [ attempt pLambda; pAtom; pList; pTuple; pRecord ]
    |> toTracable
    .>>. many (attempt (char_ws '.' >>. pIdentifier))
    |>> fun (expr, mems) -> (expr, mems) ||> List.fold (fun expr mem -> { expr with Value = RefMember (expr, mem) })
  let pTerm =
    many1 pTermItem |>> fun items -> (List.head items, List.tail items) ||> List.fold (fun f arg -> { arg with Value = Apply (f, arg) })
  let pSyntaxSugarLambdaTerm : Parser =
    char_ws '.'
    >>. sepBy1 pIdentifier (char_ws '.')
    .>>. many pTermItem
    |> toTracable
    |>> fun { Value = (mems, args); Position = pos } ->
      let trace x = { Value = x; Position = pos }
      let self = (Ref "__SELF__", mems) ||> List.fold (fun expr mem -> RefMember (trace expr, mem))
      let body = (self, args) ||> List.fold (fun f arg -> Apply (trace f, arg))
      FuncDef { Args = ["__SELF__"]; Body = trace body } |> trace
  let opp = new OperatorPrecedenceParser<Expr,unit,unit>()
  opp.TermParser <- pSyntaxSugarLambdaTerm <|> pTerm

  let infixOp  str precedence mapping = InfixOperator(str, spaces, precedence, Associativity.Left, mapping) :> Operator<_, _, _>
  let binaryOp str precedence op      = infixOp str precedence (fun x y -> { Value = BinaryOp (op, x, y); Position = None })
  let prefixOp str precedence op      = PrefixOperator (str, spaces, precedence, true, (fun x -> { Value = UnaryOp (op, x); Position = None })) :> Operator<_, _, _>
  [ binaryOp "*"  9 Mul
    binaryOp "/"  9 Div
    binaryOp "%"  9 Mod
    binaryOp "+"  8 Plus
    binaryOp "-"  8 Minus
    binaryOp "::" 7 Cons
    infixOp  "|>" 5 (fun arg f -> { arg with Value = Apply (f, arg) })
    binaryOp "<"  4 Less
    binaryOp ">"  4 Greater
    binaryOp "<=" 4 LessEq
    binaryOp ">=" 4 GreaterEq
    binaryOp "==" 3 Equal
    binaryOp "!=" 3 NotEq
    binaryOp ":?" 3 Is
    binaryOp "&&" 2 LogicalAnd
    binaryOp "||" 2 LogicalOr
    prefixOp "!" 10 LogicalNot
    prefixOp "+" 10 UnaryPlus
    prefixOp "-" 10 UnaryMinus
    InfixOperator("<-", spaces, 1, Associativity.Right, fun x y -> { Value = Substitute (x, y); Position = None }) :> Operator<_, _, _>
  ] |> List.iter opp.AddOperator
  pExprRef :=
    choice [
      pDo
      pIf
      attempt pLet
      opp.ExpressionParser |>> fun x -> x.Value
    ]
    |> toTracable
  pExpr

let parse streamName program =
  match runParserOnString (spaces >>. pExpr .>> eof) () streamName program with
  | Success (x, _, _) -> Result.Ok x
  | Failure (s, e, _) -> Result.Error s

let test() =
  parse "1" |> printfn "%A"
  parse "3.14" |> printfn "%A"
  parse "\"hello, FParsec. \"" |> printfn "%A"
  parse "hoge123piyo" |> printfn "%A"

  let expr = parse "1 + 2*(3 + 4)"
  printfn "%A" expr
