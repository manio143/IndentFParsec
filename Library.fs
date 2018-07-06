namespace IndentFParsec

open FParsec

module IndentParser =
  type Indentation = Any | Fail | Block of Position | Exact of Position
  type IndentState<'T> = { LineStart : Indentation; UserState : 'T }
  type CharStream<'T> = FParsec.CharStream<IndentState<'T>>
  type IndentParser<'T, 'UserState> = Parser<'T, IndentState<'UserState>>

  let getIndentation : IndentParser<_,_> =
    fun stream -> match stream.UserState with
                  | {LineStart = i} -> Reply i
  let getUserState : IndentParser<_,_> =
    fun stream -> match stream.UserState with
                  | {UserState = u} -> Reply u

  let putIndentation newi : IndentParser<unit, _> =
    fun stream ->
      stream.UserState <- {stream.UserState with LineStart = newi}
      Reply(Unchecked.defaultof<unit>)

  let failf fmt = fail << sprintf fmt

  let acceptable i (pos : Position) =
    match i with
    | Any -> true
    | Fail -> false
    | Block bp -> bp.Column < pos.Column
    | Exact ep -> ep.Column = pos.Column

  let nestableIn i o =
    match i, o with
    | Block i, Block o -> o.Column < i.Column
    | Block i, Exact o -> o.Column < i.Column
    | Exact i, Exact o -> o.Column = i.Column
    | Exact i, Block o -> o.Column <= i.Column
    | _, _ -> true

  let tokeniser p = parse {
    let! pos = getPosition
    let! i = getIndentation
    if acceptable i pos then return! p
    else return! failf "unexpected token at %A (indent)" pos
  }

  let nestP i o p = parse {
    do! putIndentation i
    let! x = p
    do! notFollowedBy (tokeniser anyChar) <?> (sprintf "unterminated %A" i)
    do! putIndentation o
    return x
  }

  let nest indentor p = parse {
    let! outerI = getIndentation
    let! curPos = getPosition
    let innerI = indentor curPos
    if nestableIn innerI outerI
    then return! nestP innerI outerI p
    else return! nestP Fail outerI p
  }

  let nestWithPos indentor pos p = parse {
    let! outerI = getIndentation
    let innerI = indentor pos
    if nestableIn innerI outerI
    then return! nestP innerI outerI p
    else return! nestP Fail outerI p
  }

  let neglectIndent p = parse {
    let! o = getIndentation
    do! putIndentation Any
    let! x = p
    do! putIndentation o
    return x
  }
