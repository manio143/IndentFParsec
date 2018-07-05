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
  let spaces<'i, 'u> : IndentParser<unit, 'u> = tokeniser spaces

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

  let keyword str = tokeniser (pstring str) >>? tokeniser (nextCharSatisfiesNot (fun c -> isLetter c || isDigit c) <?> str)

  let integer<'i, 'u> : IndentParser<int32, 'u> = spaces >>. tokeniser pint32

  let rec loop = tokeniser <| parse {
      do! spaces
      let! expStart = getPosition
      do! keyword "loop"
      let! id = identifier
      let! min = integer
      let! max = integer
      let! stmts = nestWithPos BlockAfter expStart (many' (statement .>> spaces))
      return Loop(id, min, max, stmts)
    }
  and print = tokeniser <| parse {
      do! keyword "print"
      let! id = identifier
      return Print id
    }
  and statement = spaces >>. (print <|> loop)
  
  let statements = many' (statement .>> spaces)

  let document = statements .>> spaces .>> eof
  let testParse str =
      match runParserOnString document {LineStart = Any; UserState = ()} "" str with
      | Success(result, _, _)   ->
          printfn "Success: %A" result
          Some result
      | Failure(errorMsg, p, _) ->
          printfn "Failure: %s\nREASONS: %A" errorMsg (p.UserState)
          None
