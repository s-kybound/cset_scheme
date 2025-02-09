let rec repl () =
  print_string "cset_scheme> ";
  flush stdout;
  try
    let input = read_line () in 
    if input = "exit" then
      exit 0
    else
      begin
        try
          let sexp = Sexplib.Sexp.of_string input in
          let parsed_sexp = Parse.parse sexp in
          let res = Cset.eval parsed_sexp in
          print_endline (Sexplib.Sexp.to_string_hum (Parse.map_back res))
        with
        | Failure msg -> print_endline ("Error: " ^ msg) (* execution errors *)
        | Sexplib.Sexp.Parse_error msg -> print_endline ("Parse error: " ^ msg.err_msg) (* parsing errors *)
        | _ -> print_endline "Unknown error occurred.";
      end;
      repl ()
  with End_of_file -> exit 0;;