module FunnyScript.Parser
open System
open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

//type Result = Result<Expr, string * ParserError>
type Result = Result<Expr, string>
type Parser = Parser<Expr, unit>

let private trace parser =
  let lncol (pos : Position) = int pos.Line, int pos.Column
  getPosition .>>. parser .>>. getPosition
  |>> fun ((pos1, x), pos2) -> x, { FilePath = pos1.StreamName; LineCol1 = lncol pos1; LineCol2 = lncol pos2 }

let pLiteralNumber =
  let numfmt =
    //NumberLiteralOptions.AllowMinusSign |||
    //NumberLiteralOptions.AllowPlusSign |||
    NumberLiteralOptions.AllowFraction |||
    NumberLiteralOptions.AllowExponent
  numberLiteral numfmt "number" .>> spaces
  |>> (fun x -> (if x.IsInteger then box (int32 x.String) else box (float x.String)) |> Obj)

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
  |>> (String.concat "" >> box >> Obj)

let pIdentifier =
  choice [
    regex @"\w(\w|\d)*" .>> spaces
    many (noneOf "`") |> between (skipChar '`') (skipChar '`') .>> spaces |>> (List.toArray >> String)
    pstring "@" .>> spaces
  ]

let pAtom =
  choice [
    skipString "true"  .>> spaces |>> (fun () -> box true  |> Obj)
    skipString "false" .>> spaces |>> (fun () -> box false |> Obj)
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
      | ((_, expr1), expr2)         -> Let (null, expr1, expr2)
  let pDo =
    str_ws "do" >>. pExpr .>> char_ws ';' .>>. opt pExpr
    |>> fun (expr1, expr2) -> Let (null, expr1, expr2 |> Option.defaultValue (Obj null))
  let pOpen =
    str_ws "open" >>. pExpr .>> char_ws ';' .>>. pExpr
    |>> Open
  let pIf =
    (opt (char_ws '|') .>> char_ws '?') >>. pExpr .>> str_ws "=>" .>>. pExpr .>>. opt (char_ws '|' >>. pExpr)
    |>> fun ((cond, expr1), expr2) -> If (cond, expr1, expr2 |> Option.defaultValue (Ref "unmatched"))
  let pLambda =
    choice [
      pIdentifier |>> (fun x -> [x])
      between_ws '(' ')' (sepByComma pIdentifier)
    ]
    .>> str_ws "->" .>>. pExpr
    |>> (fun (args, body) -> FuncDef { Args = args; Body = body })
  let pLambda2 =
    char_ws '$' >>. pExpr |>> fun expr -> FuncDef { Args = ["@"]; Body = expr }
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
      | [] -> Obj null
      | [expr] -> expr
      | tuple  -> tuple |> List.toArray |> NewList
  let pTermItem =
    choice [ attempt pLambda; pLambda2; pAtom; pList; pTuple; pRecord ]
    .>>. many (attempt (char_ws '.' >>. pIdentifier))
    |>> fun (expr, mems) -> (expr, mems) ||> List.fold (fun expr mem -> RefMember (expr, mem))
  let pTerm =
    many1 pTermItem |>> fun items -> (List.head items, List.tail items) ||> List.fold (fun f arg -> Apply (f, arg))
  let pSyntaxSugarLambdaTerm : Parser =
    char_ws '.'
    >>. sepBy1 pIdentifier (char_ws '.')
    .>>. many pTermItem
    |>> fun (mems, args) ->
      let self = (Ref "__SELF__", mems) ||> List.fold (fun expr mem -> RefMember (expr, mem))
      let body = (self, args) ||> List.fold (fun f arg -> Apply (f, arg))
      FuncDef { Args = ["__SELF__"]; Body = body }
  let opp = new OperatorPrecedenceParser<Expr,unit,unit>()
  opp.TermParser <- pSyntaxSugarLambdaTerm <|> pTerm

  let infixOp  str precedence mapping = InfixOperator(str, spaces, precedence, Associativity.Left, mapping) :> Operator<_, _, _>
  let binaryOp str precedence = infixOp str precedence (fun x y -> Apply (Apply (Ref str, x), y))
  let prefixOp str precedence = PrefixOperator (str, spaces, precedence, true, (fun x -> Apply (Ref ("~" + str), x))) :> Operator<_, _, _>
  // ↑単項演算子の場合は先頭に '~' を付けた識別子とする
  [ binaryOp "*"  9
    binaryOp "/"  9
    binaryOp "%"  9
    binaryOp "+"  8
    binaryOp "-"  8
    binaryOp "::" 7
    binaryOp "|>" 5
    binaryOp "|?>" 5
    infixOp  "|!>" 5 (fun arg handler -> OnError (arg, handler))
    binaryOp "<"  4
    binaryOp ">"  4
    binaryOp "<=" 4
    binaryOp ">=" 4
    binaryOp "==" 3
    binaryOp "!=" 3
    binaryOp ":?" 3
    infixOp "&&" 2 (fun x y -> LogicalAnd (x, y)) // 論理演算は、左オペランドの値によっては右オペランドの式を評価してはいけない
    infixOp "||" 2 (fun x y -> LogicalOr  (x, y)) // 場合があるので、他の演算子のように binaryOp で定義することは出来ない
    prefixOp "!" 10
    prefixOp "+" 10
    prefixOp "-" 10
    InfixOperator("<-", spaces, 1, Associativity.Right, fun x y -> Substitute (x, y)) :> Operator<_, _, _>
  ] |> List.iter opp.AddOperator
  pExprRef :=
    choice [
      pDo
      pIf
      pOpen
      attempt pLet
      opp.ExpressionParser
    ]
    |> trace |>> Trace
  pExpr

let private removeComments (program : string) =
  program.Split '\n'
  |> Seq.map (fun (s : string) -> let i = s.IndexOf "//" in if 0 <= i && i < s.Length then s.Substring (0, i) else s)  // コメントの除去
  |> String.concat "\n"

let parse streamName program =
  removeComments program
  |> runParserOnString (spaces >>. pExpr .>> eof) () streamName
  |> function
    | Success (x, _, _) -> Result.Ok x
    | Failure (s, e, _) -> Result.Error s

let test() =
  parse "1" |> printfn "%A"
  parse "3.14" |> printfn "%A"
  parse "\"hello, FParsec. \"" |> printfn "%A"
  parse "hoge123piyo" |> printfn "%A"

  let expr = parse "1 + 2*(3 + 4)"
  printfn "%A" expr
