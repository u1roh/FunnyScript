﻿module FunnyScript.Parser
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

let private str_ws   s = skipString s >>. spaces
let private char_ws  c = skipChar   c >>. spaces
let private between_ws c1 c2 p = between (char_ws c1) (char_ws c2) p
let private sepByComma p = sepBy p (char_ws ',')

let pLiteralNumber =
  let numfmt =
    //NumberLiteralOptions.AllowMinusSign |||
    //NumberLiteralOptions.AllowPlusSign |||
    NumberLiteralOptions.AllowFraction |||
    NumberLiteralOptions.AllowExponent
  numberLiteral numfmt "literal number" .>> spaces
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
  |>> String.concat ""
  <?> "literal string"

let reservedWords =
  [ "do"; "if"; "else"; "in" ] |> Set.ofList

let pIdentifier, pIdentifierDef, pIdentifierRef =
  let iddef =
    choice [
      attempt (regex @"[_\p{Ll}\p{Lu}\p{Lt}\p{Lo}]([_\p{Ll}\p{Lu}\p{Lt}\p{Lo}\p{Nd}\p{Lm}])*" .>> spaces
        >>= fun s -> if reservedWords |> Set.contains s then fail (sprintf "'%s' is reserved word." s) else (fun stream -> Reply s))
      many (noneOf "`") |> between (skipChar '`') (skipChar '`') .>> spaces |>> (List.toArray >> String)
    ] <?> "identifier"
  let id = iddef >>= function "_" -> fail "cannot refer '_'" | id -> preturn id
  id, iddef, id <|> (pstring "@" .>> spaces)

let pAtom =
  choice [
    skipString "true"  .>> spaces |>> (fun () -> box true  |> Obj)
    skipString "false" .>> spaces |>> (fun () -> box false |> Obj)
    pLiteralNumber
    pLiteralString |>> (box >> Obj)
    pIdentifierRef |>> Ref
  ]

let pExpr =
  let pExpr, pExprRef = createParserForwardedToRef<Expr, unit>()

  let pPattern =
    let pPattern, pPatternRef = createParserForwardedToRef<PatternExpr, unit>()
    let sepByNone optionList =
      ([[]], optionList) ||> List.fold (fun lists item ->
        match item with
        | Some item -> (item :: lists.Head) :: lists.Tail
        | _ -> [] :: lists)
    let pPatternInBracket =
      choice [
        between_ws '{' '}' (sepEndBy (pIdentifier .>> str_ws ":=" .>>. pPattern) (char_ws ';')) |>> XRecord
        between_ws '(' ')' (sepByComma pPattern) |>> function [p] -> p | items -> XTuple items
        between_ws '[' ']' (sepByComma ((pPattern |>> Some) <|> (str_ws "..." |>> fun _ -> None)))
          >>= (sepByNone >> function
            | [ptns] -> preturn (XArray (ptns, None))
            | [ptns2; ptns1] -> preturn (XArray (ptns1, Some ptns2))
            | _ -> fail "invalid array pattern")
      ]
    let pTermExpr =
      pAtom <|> (between_ws '(' ')' pExpr)
      .>>. (opt (char_ws '.' >>. sepBy1 pIdentifier (char_ws '.')) |>> Option.defaultValue [])
      |>> fun (expr, mems) -> mems |> List.fold (fun expr mem -> RefMember (expr, mem)) expr
    let pPatternTerm =
      choice [
        pPatternInBracket
        char_ws '#' >>. pTermExpr .>>. opt pPattern |>> fun (case, pat) -> XCase (case, pat |> Option.defaultValue PatternExpr.Empty)
        char_ws ':' >>. pTermExpr |>> XTyped
      ]
    pPatternRef :=
      choice [
        pPatternTerm
        pIdentifierDef .>>. opt pPatternTerm |>> fun (name, pat) -> XNamed (name, pat |> Option.defaultValue XAny)
      ] <?> "pattern"
    pPattern

  let pLet =
    attempt (opt pIdentifierDef .>> str_ws ":=") .>>. pExpr .>> char_ws ';' .>>. pExpr
    |>> function
      | ((Some name, expr1), expr2) -> Let (name, expr1, expr2)
      | ((_, expr1), expr2)         -> Let (null, expr1, expr2)
  let pDo =
    str_ws "do" >>. (between_ws '{' '}' (sepEndBy pExpr (char_ws ';')) <|> (pExpr .>> char_ws ';' |>> fun x -> [x])) .>>. opt pExpr
    |>> fun (expr1, expr2) ->
      List.foldBack (fun expr tailExpr -> Let (null, expr, tailExpr)) expr1 (expr2 |> Option.defaultValue (Obj null))
  let pOpen =
    str_ws "open" >>. pExpr .>> char_ws ';' .>>. pExpr
    |>> Open
  let pLoad =
    str_ws "load" >>. pLiteralString .>> char_ws ';' .>>. pExpr
    |>> Load
  let pIf =
    many1 (str_ws "if" >>. between_ws '(' ')' pExpr .>>. pExpr) .>>. opt (str_ws "else" >>. pExpr)
    |>> fun (ifList, elseExpr) ->
      elseExpr |> Option.defaultValue (Ref "unmatched")
      |> List.foldBack (fun (cond, thenExpr) elseExpr -> If (cond, thenExpr, elseExpr)) ifList
  let pLambda =
    attempt (pPattern .>> str_ws "->") .>>. pExpr
    |>> (fun (args, body) -> FuncDef { Args = args; Body = body })
    <?> "lambda"
  let pLambda2 =
    attempt (skipChar '|' .>> followedBy (noneOf ">|!?") .>> spaces) >>. pExpr
    |>> fun expr -> FuncDef { Args = XNamed ("@", XAny); Body = expr }
    <?> "lambda"
  let pRecord =
    let pRecItem = pIdentifier .>> str_ws ":=" .>>. pExpr
    between_ws '{' '}' (sepEndBy pRecItem (char_ws ';'))
    |>> NewRecord
    <?> "record"
  let pArray =
    between_ws '[' ']' (sepByComma pExpr |>> (List.toArray >> NewArray))
    <?> "array"
  let pTuple =
    between_ws '(' ')' (sepByComma pExpr)
    |>> function
      | [] -> Obj null
      | [expr] -> expr
      | tuple  -> tuple |> List.toArray |> NewArray
    <?> "tuple"
  let pCase =
    char_ws '#' >>. opt pPattern |>> NewCase
    <?> "case"
  let pRange =
    between (pchar '~') (pchar '~')
      ((pchar '[' <|> pchar '(') .>> spaces
      .>>. pExpr .>> char_ws ',' .>>. pExpr
      .>>. (pchar ']' <|> pchar ')'))
    .>> spaces
    <?> "interval"
    |>> fun (((brace1, lower), upper), brace2) ->
      let lower = { Expr = lower; IsOpen = brace1 = '(' }
      let upper = { Expr = upper; IsOpen = brace2 = ')' }
      Interval (lower, upper)
  let pTermItem =
    choice [ pLambda; pLambda2; pAtom; pArray; pTuple; pRecord; pCase; pRange ]
    .>>. many (attempt (char_ws '.') >>. pIdentifier)
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
      FuncDef { Args = XNamed ("__SELF__", XAny); Body = body }
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

  [ "∪"; "∩"; "∈"; "∋"; "⊂"; "⊃"; "in" ]
  |> List.map (fun op -> binaryOp op 1)
  |> List.iter opp.AddOperator

  pExprRef :=
    choice [
      pDo
      pIf
      pOpen
      pLoad
      pLet
      opp.ExpressionParser
    ]
    |> trace |>> Trace
  pExpr

let pModule =
  many (pIdentifier .>> str_ws ":=" .>>. pExpr .>> char_ws ';')

let private removeComments (program : string) =
  let sb = Text.StringBuilder()
  let mutable state = 0
  let mutable start = 0
  for i = 0 to program.Length - 1 do
    let ch = program.[i]
    state <-
      match state with
      | 0 -> if ch = '/' then 1 else 0
      | 1 ->
        match ch with
        | '*' -> sb.Append(program.Substring (start, i-1 - start)).Append ' ' |> ignore; 2
        | '/' -> sb.Append(program.Substring (start, i-1 - start)).Append ' ' |> ignore; 4
        | _   -> 0
      | 2 -> if ch = '*' then 3 else 2
      | 3 ->
        match ch with
        | '/' -> start <- i + 1; 0
        | '*' -> 3
        | _   -> 2
      | _ ->  if ch = '\n' then start <- i + 1; 0 else 4
  if (state = 0 || state = 1) && start < program.Length
    then sb.Append (program.Substring (start, program.Length - start)) |> ignore
  sb.ToString()

let parse streamName program =
  removeComments program
  |> runParserOnString (spaces >>. pExpr .>> eof) () streamName
  |> function
    | Success (x, _, _) -> Result.Ok x
    | Failure (s, e, _) -> Result.Error s

let parseModule streamName program =
  removeComments program
  |> runParserOnString (spaces >>. pModule .>> eof) () streamName
  |> function
    | Success (x, _, _) -> FunnyScript.Ok x
    | Failure (s, e, _) -> FunnyScript.Error s

let test() =
  parse "1" |> printfn "%A"
  parse "3.14" |> printfn "%A"
  parse "\"hello, FParsec. \"" |> printfn "%A"
  parse "hoge123piyo" |> printfn "%A"

  let expr = parse "1 + 2*(3 + 4)"
  printfn "%A" expr
