#if INTERACTIVE
#r "bin/Debug/netstandard2.0/FParsecCS.dll"
#r "bin/Debug/netstandard2.0/FParsec.dll"
#r "bin/Debug/netstandard2.0/IdentFParsec.dll"
#else
namespace IndentFParsec
#endif

#nowarn "40"

module Test =
  open FParsec
  open IndentFParsec.IndentParser

  type Identifier = string

  type Statement = Loop of Identifier * int * int * Statement list
                 | Print of Identifier

  //let whitespace<'i, 'u> : IndentParser<unit, 'i, 'u> = tokeniser spaces
  //let spaces<'i, 'u> : IndentParser<unit, 'i, 'u> = tokeniser (skipMany (pchar ' '))
  let rec many1' p = parse {
    let! x = p
    let! xs = attempt (many1' p) <|> preturn []
    return x::xs
  }
  let many' p = many1' p <|> preturn []

  let stringOf p = many' p |>> (List.map string >> List.fold (+) "")

  let identifier = parse {
    do! spaces
    let! f = tokeniser asciiLetter
    let! t = stringOf (asciiLetter <|> digit)
    return string f + t
  }

  let keyword str = tokeniser (pstring str >>? (nextCharSatisfiesNot (fun c -> isLetter c || isDigit c) <?> str))

  let integer<'i, 'u> : IndentParser<int32, 'u> = tokeniser pint32

  let rec loop = parse {
      do! keyword "loop"
      let! id = greater >>. identifier
      let! min = greater >>. integer
      let! max = greater >>. integer
      let! stmts = exact >>. loopBlock
      return Loop(id, min, max, stmts)
    }
  and loopBlock = parse {
      do! indent
      return! greater >>. statements
    }
  and print = parse {
      do! keyword "print"
      let! id = greater >>. identifier
      return Print id
    }
  and statement = exact >>. (print <|> loop)
  and statements = blockOf statement

  let document = statements .>> atLeast .>> eof

  let testParse str =
      match runParser document () str with
      | Success(result, _, _)   ->
          printfn "Success: %A" result
          Some result
      | Failure(errorMsg, p, _) ->
          printfn "Failure: %s\nREASONS: %A" errorMsg (p.UserState)
          None
