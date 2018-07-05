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
  open IndentFParsec.SampleIndentations

  type Identifier = string

  type Statement = Loop of Identifier * int * int * Statement list
                 | Print of Identifier

  //let whitespace<'i, 'u> : IndentParser<unit, 'i, 'u> = tokeniser spaces
  //let spaces<'i, 'u> : IndentParser<unit, 'i, 'u> = tokeniser (skipMany (pchar ' '))
  let spaces<'i, 'u> : IndentParser<unit, 'i, 'u> = tokeniser spaces

  let identifier = spaces >>. tokeniser asciiLower |>> string 
  let keyword str = spaces >>. tokeniser (pstring str) >>? tokeniser (nextCharSatisfiesNot (fun c -> isLetter c || isDigit c) <?> str)

  let integer<'i, 'u> : IndentParser<int32, 'i, 'u> = spaces >>. tokeniser pint32

  let rec loop = tokeniser <| parse {
      do! keyword "loop"
      let! id = identifier
      let! min = integer
      let! max = integer
      let! stmts = statementBlock
      return Loop(id, min, max, stmts)
    }
  and print = tokeniser <| parse {
      do! keyword "print"
      let! id = identifier
      return Print id
    }
  and statement = spaces >>. (print <|> loop)
  and statementBlock =
      let rec manyStatements = parse {
        let! stm = nest LineFold statement
        let! stmts = attempt manyStatements <|> preturn []
        return (stm :: stmts)
      }
      nest Block manyStatements

  let document = statementBlock .>> spaces .>> eof
  let testParse str =
      match runParserOnString document (Indent(haskellLikeIndentation, Neglect, ())) "" str with
      | Success(result, _, _)   ->
          printfn "Success: %A" result
          Some result
      | Failure(errorMsg, p, _) ->
          printfn "Failure: %s\nREASONS: %A" errorMsg (p.UserState)
          None
