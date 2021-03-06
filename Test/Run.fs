namespace IndentFParsec
module Entry =
  open Test

  [<EntryPoint>]
  let main args =
    do ignore <| testParse "print k\nprint d\n"
    do ignore <| testParse @"
  loop i 1 10
    loop k 1 10
     print k1
     print k2
     print k3
    print i
  print j
  "
    do ignore <| testParse @"
    loop i 1 10  
      print i1
      print i2
    "
    do ignore <| testParse @"
    loop i 1 2
      case i of
      | 1 -> loop k 1 5
              print k
      | 2 ->
        loop j 1 10
          print i
          print j
    "
    0