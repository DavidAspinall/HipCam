module Neoexport = struct

  open Neoprinter;;
  open Neograph;;

let create_new_filename dir suggestion =
  let rec create i = 
    let name = 
      match i with
          0 -> dir ^ "/" ^ suggestion 
        | _ -> dir ^ "/" ^ suggestion ^ "_" ^ (string_of_int i) 
    in
    if Sys.file_exists name then create (i+1) else name 
  in create 0;;

let escape_file name = "\"" ^ (String.escaped name) ^ "\"" (* FIXME: approximates shell escapes *)

let copy_dir old_dir new_dir =
  let _ = Sys.command ("cp -r "^(escape_file old_dir)^" "^(escape_file new_dir)) in
  ();;

let save_textfile file string =
  let channel = open_out file in
  output_string channel string;
  close_out channel;;

let neoexport hyps_and_goal name jg = 
  let defgraph = (neoprint_start();
                  neoprint_create hyps_and_goal name jg;
                  neoprint_stop()) in 
  let dir = (Sys.getcwd ()) ^ "/hiproofs/neo/output" in
  if (not (Sys.file_exists dir && Sys.is_directory dir)) then failwith ("no such directory: "^dir) else ();
  let tmp_dir = create_new_filename dir name in
  let tmp_file = tmp_dir^"/creategraph.neo4j" in
  save_textfile tmp_file defgraph;
  print_string ("exported graph to: "^tmp_file);;
  
end
