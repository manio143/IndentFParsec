namespace IndentFParsec
module Entry =
  open Test

  [<EntryPoint>]
  let main args =
    do ignore <| testParse "print k\nprint d"
    do ignore <| testParse @"
  loop i 1 10
                loop k 1 10
                            print k1
                            print k2
                            print k3
                print i
  print j
  "
    0