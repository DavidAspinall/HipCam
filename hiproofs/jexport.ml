module Jexport = struct

  open Jprinter;;
  open Jgraph;;

let create_new_filename dir suggestion =
  let rec create i = 
    let name = 
      match i with
          0 -> dir ^ "/" ^ suggestion 
        | _ -> dir ^ "/" ^ suggestion ^ "_" ^ (string_of_int i) 
    in
    if Sys.file_exists name then create (i+1) else name 
  in create 0;;

let escape_file name = (jprint_start(); jprint_fstring name; jprint_stop());;

let copy_dir old_dir new_dir =
  let _ = Sys.command ("cp -r "^(escape_file old_dir)^" "^(escape_file new_dir)) in
  ();;

let save_textfile file string =
     let channel = open_out file in
     output_string channel string;
     close_out channel;;

let show_in_browser file =
  let cmd = JGRAPH_BROWSER_COMMAND in
  let i = String.index cmd '$' in
  let cmd = (String.sub cmd 0 i)^
    (escape_file file)^
    (String.sub cmd (i+1) ((String.length cmd) - (i+1))) in
  let _ = Sys.command cmd in
  ();;

let jexport hyps_and_goal name jg = 
  let defgraph = (jprint_start();
                  print_defgraph hyps_and_goal name jg;
                  jprint_stop()) in 
  let dir = (Sys.getcwd ()) ^ "/hiproofs/html/output" in
  if (not (Sys.file_exists dir && Sys.is_directory dir)) then failwith ("no such directory: "^dir) else ();
  let tmp_dir = create_new_filename dir name in
  copy_dir (dir^"/template") tmp_dir;
  save_textfile (tmp_dir^"/defgraph.js") defgraph;
  let index_html = tmp_dir^"/index.html" in
  print_string ("exported graph to: "^index_html);
  show_in_browser index_html;;
  
end
