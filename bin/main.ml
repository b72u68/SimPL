open SimPL

let filename = Sys.argv.(1)

let _ =
  let filetype = Filename.extension filename in
  if filetype <> ".spl" then
    failwith
      (Printf.sprintf "Unsupported file type '%s'. Expected '.spl'." filetype)
  else
    let prog = Parseutil.parse ~source:FILE filename in
    let _ = Typecheck.typecheck prog in
    let _, state = Eval.big_step (prog, Types.VarMap.empty) in
    print_endline (Print.print_state state)
