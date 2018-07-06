namespace IndentFParsec

#nowarn "40"

module Test =
  open FParsec
  open IndentFParsec.IndentParser

  type Identifier = string

  type Statement = Loop of Identifier * int * int * Statement list
                 | Print of Identifier

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
      let! pos = getPosition
      do! exact pos (keyword "loop")
      let! id = greater pos identifier
      let! min = greater pos integer
      let! max = greater pos integer
      let! stmts = greater pos statements
      return Loop(id, min, max, stmts)
    }
  and print = parse {
      let! pos = getPosition
      do! exact pos (keyword "print")
      let! id = greater pos identifier
      return Print id
    }
  and statement = (attempt print <|> loop)
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
