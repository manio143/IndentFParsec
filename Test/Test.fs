namespace IndentFParsec

#nowarn "40"

module Test =
  open FParsec
  open IndentFParsec.IndentParser

  type Identifier = string

  type Statement = Loop of Identifier * int * int * Statement list
                 | Print of Identifier
                 | Case of Identifier * Pattern list
  and  Pattern = Pattern of int * Statement list

  let stringOf p = many p |>> (List.map string >> List.fold (+) "")
  
  let identifier = parse {
    do! spaces
    let! f = tokeniser asciiLetter
    let! t = stringOf (asciiLetter <|> digit)
    return string f + t
  }

  let keyword str = tokeniser (pstring str >>? (nextCharSatisfiesNot (fun c -> isLetter c || isDigit c) <?> str))
  let op str = tokeniser (pstring str >>? (nextCharSatisfiesNot (isAnyOf "!@#$%^&*()-+=?/><|") <?> str))

  let integer<'u> : IndentParser<int32, 'u> = tokeniser pint32

  let rec loop = parse {
      let! pos = getPosition
      do! exact pos (keyword "loop")
      let! id = greater pos identifier
      let! min = greater pos integer
      let! max = greater pos integer
      let! stmts = newline >>. greater pos statements
      return Loop(id, min, max, stmts)
    }
  and print = parse {
      let! pos = getPosition
      do! exact pos (keyword "print")
      let! id = greater pos identifier
      return Print id
    }
  and case = parse {
      let! pos = getPosition
      do! exact pos (keyword "case")
      let! id = greater pos identifier
      do! greater pos (keyword "of")
      let! pats = newline >>. atLeast pos (blockOf pattern)
      return Case(id, pats)
    }
  and pattern = parse {
      let! pos = getPosition
      do! exact pos (op "|")
      let! value = greater pos integer
      do! greater pos (op "->")
      let! stmts = greater pos statements
      return Pattern(value, stmts)
    }
  and statement = (attempt print <|> attempt case <|> loop)
  and statements = blockOf statement

  let document = spaces >>. statements .>> spaces .>> eof

  let testParse str =
      match runParser document () str with
      | Success(result, _, _)   ->
          printfn "Success: %A" result
          Some result
      | Failure(errorMsg, p, _) ->
          printfn "Failure: %s\nREASONS: %A" errorMsg (p.UserState)
          None
