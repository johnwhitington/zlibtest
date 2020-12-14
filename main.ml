(* Read file from argv[1], decompress, write to argv[2] *)
let go i o = ()

let _ =
  match Sys.argv with
    [|_; input; output|] -> go input output
  | _ -> prerr_string "Format: main in out\n"; exit 2
