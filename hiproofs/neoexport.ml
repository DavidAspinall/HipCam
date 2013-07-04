module Neoexport = struct

  let NEO_DB_SHELL_COMMAND = ref "";
  (* on my machine: /usr/local/Cellar/neo4j/community-1.9.1-unix/bin/neo4j-shell *)

  open Neoprinter;;
  open Neograph;;
  open Jexport;;

  (* TODO: this pops up an OS X dialogue! Find better way to connect, via REST or some API *)
  let pipe_to_db file = 
    let cmd = (!NEO_DB_SHELL_COMMAND) in
    if (cmd != "") then
      let retval = Sys.command (cmd ^ " -file " ^ file ^ " > /dev/null") in 
      if retval > 0 then 
	failwith("Sending CREATE command to database failed.") else ();
    else ();;

  let neoexport hyps_and_goal name jg = 
    let defgraph = (neoprint_start();
                    neoprint_create hyps_and_goal name jg;
                    neoprint_stop()) in 
    let dir = (Sys.getcwd ()) ^ "/hiproofs/neo/output" in
    if (not (Sys.file_exists dir && Sys.is_directory dir)) then
      failwith ("no such directory: "^dir) else ();
    let tmp_file = create_new_filename dir (name^".neo4j") in
    save_textfile tmp_file defgraph;
    pipe_to_db tmp_file;
    print_string ("exported graph to: "^tmp_file);;
  
end
