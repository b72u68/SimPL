open SimPL.Parseutil
open SimPL.Typecheck
open SimPL.Print

let filename = Sys.argv.(1)

let _ =
  let filetype = Filename.extension filename in
  if filetype <> ".spl" then
    failwith
      (Printf.sprintf "Unsupported file type '%s'. Expected '.spl'." filetype)
  else
    let prog = parse ~source:FILE filename in
    let _ = typecheck prog in
    print_endline (print_stmt prog)
