let _ =
  let file = Sys.argv.(1) in
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  Impparser.prog Implexer.token lexbuf
