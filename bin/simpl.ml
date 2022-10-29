open SimPL

let filename = Sys.argv.(1);;

let _ =
    let filetype = Filename.extension filename in
    if filetype <> ".spl" then failwith (Printf.sprintf "Unsupported file type '%s'. Expected '.spl'." filetype)
    else Parse.parse filename
;;
