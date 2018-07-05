namespace IndentFParsec

open FParsec

module IndentParser =
  type Indentation<'i> = {
    always : 'i;
    never : 'i;
    acceptable : 'i -> Position -> bool;
    nestableIn : 'i -> 'i -> bool
  }
  type IndentState<'T, 'i> = Indent of Indentation<'i> * 'i * UserState: 'T

  type CharStream<'T, 'i> = FParsec.CharStream<IndentState<'T, 'i>>
  type IndentParser<'T, 'i, 'UserState> = Parser<'T, IndentState<'UserState, 'i>>

  let getIndentation : IndentParser<_, 'i, 'a> =
    fun stream -> match stream.UserState with
                  | Indent (ind, i, _) -> Reply((ind, i))
  let getUserState : IndentParser<_, 'i, 'a> =
    fun stream -> match stream.UserState with
                  | Indent (_, _, u) -> Reply(u)

  let putIndentation newi : IndentParser<unit, 'i, 'a> =
    fun stream -> match stream.UserState with
                  | Indent (ind, _, u) -> 
                      stream.UserState <- Indent (ind, newi, u)
                      Reply(Ok, (), null)

  let failf fmt = fail << sprintf fmt

  let tokeniser p = parse {
    let! pos = getPosition
    let! indentation, i = getIndentation
    if indentation.acceptable i pos then return! p
    else return! failf "unexpected token at %A" pos
  }

  let nestP i o p = parse {
    do! putIndentation i
    let! x = p
    return! ((tokeniser anyChar >>. preturn x) <?> (sprintf "unterminated %A" i)) <|> parse {
      do! putIndentation o
      return x
    }
  }

  let nestii indentor p ind outerI = parse {
    let! curPos = getPosition
    let innerI = indentor curPos
    if ind.nestableIn innerI outerI
    then return! nestP innerI outerI p
    else return! nestP ind.never outerI p
  }

  let nest indentor p = parse {
    let! indentation, outerI = getIndentation
    return! nestii indentor p indentation outerI
  }

  let nestI indentor p pos = parse {
    let! indentation, outerI = getIndentation
    return! nestii indentor p indentation pos
  }

  let neglectIndent p = parse {
    let! indentation, o = getIndentation
    do! putIndentation indentation.always
    let! x = p
    do! putIndentation o
    return x
  }

module SampleIndentations =
  open IndentParser
  type HaskellLike = Never | Neglect | Block of Position | LineFold of Position
      with
          override x.ToString() =
            match x with
            | Never -> "empty block"
            | Neglect -> "unindented chunk"
            | Block bp -> sprintf "block started at %A" bp
            | LineFold lp -> sprintf "line fold started at %A" lp

  let haskellLikeIndentation = {
    never = Never;
    always = Neglect;
    acceptable = function
      | Never         -> fun _ -> false
      | Neglect       -> fun _ -> true
      | (Block bp)    -> fun p -> bp.Column <= p.Column
      | (LineFold lp) -> fun p -> lp.Column < p.Column || lp = p
    nestableIn = function
      | (Block i) -> function
          | (Block o) -> o.Column < i.Column
          | (LineFold o) -> o.Column < i.Column
          | _ -> true
      | (LineFold i) -> function
          | (Block o) -> o.Column <= i.Column
          | (LineFold o) -> o.Column < i.Column
          | _ -> true
      | _ -> fun _ -> true
  }