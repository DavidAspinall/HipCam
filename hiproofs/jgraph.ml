(* ========================================================================== *)
(* jgraph.ml                                                                  *)
(* Exporting hiproofs to javascript visualisation.                            *)
(*                                                                            *)
(* by Steven Obua                                                             *)
(* Copyright (c) University of Edinburgh, 2012                                *)
(* Copyright (c) Steven Obua, 2012                                            *)
(* ========================================================================== *)

module Jgraph = struct

  open Jprinter;;
    
  type jid = int;;
  type label = Hiproofs.label;;

  type 'a jnode =
      Jatomic of jid * 'a * label                 (* Goal resolved by atomic tactic *)
    | Jbox of jid * 'a * label * 'a jgraph        (* Box *)
    | Jedge of jid * jid                          (* Edge *)
  and 'a jgraph = 'a jnode list;;
  
(*  let rec hiproof_goalids h =
  match h with 
      Hactive id -> [id]
    | Hatomic (id, _) -> [id]
    | Hidentity id -> [id]
    | Hlabelled (l, g) -> hiproof_goalids g
    | Hsequence (h1, h2) ->
        union (hiproof_goalids h1) (hiproof_goalids h2)
    | Htensor hs -> unions (map hiproof_goalids hs)
    | Hempty -> [];;

let hiproof_jgraph h =
  let current_jid = ref 1 in
  let next_jid () = (let j = !current_jid in current_jid := j + 1; j) in
  let jids_of_goalids = map (fun gid -> (gid, next_jid ())) (hiproof_goalids h) in
  let jid_of_gid gid = assoc gid jids_of_goalids in
  let rec make h = 
    match h with
        Hactive id -> [Jactive (jid_of_gid id, id)]
      | Hatomic (id, _) -> [Jatomic (jid_of_gid id, id)]
      | Hidentity _ -> []
      | Hlabelled (l, g) -> 
          (match hiproof_ins g with
              [gid] -> [Jbox (next_jid(), gid, l, make g)]
            | _ -> make g)
      | Hsequence (h1, h2) ->
          let idids = zip (hiproof_outs h1) (hiproof_ins h2) in
          let idids' = filter (fun (id1,id2) -> (id1 > 0) & (id2 > 0)) idids in
          (make h1) @ (make h2) @ 
          (map (fun (id1, id2) -> Jedge (jid_of_gid id1, jid_of_gid id2)) idids')
      | Htensor hs -> flat (map make hs)
      | Hempty -> []
  in
  make h;;*)

let jindent = "    ";;

let rec print_jnode hyps_and_goal parent jnode =
  let comma () = jprint_string ", " in
  let print_hyps hyps =
    jprint_string "[";
    (match hyps with
        [] -> ()
      | [h] -> jprint_fstring h 
      | (h::hs) -> 
          jprint_fstring h; 
          List.iter (fun h -> comma (); jprint_fstring h) hs);
    jprint_string "]"
  in
  let print_node parent jid goal label =
    let (hyps, goal) = hyps_and_goal goal in
    jprint_string jindent; jprint_string "defineNode (gs, ";
    (match parent with None -> jprint_string "null" | Some jid -> jprint_int jid);
    comma (); jprint_int jid;
    comma (); print_hyps hyps; 
    comma (); jprint_fstring goal;
    comma (); 
    jprint_label label;
    jprint_string ");\n"
  in
  let print_edge id1 id2 =
    jprint_string jindent; jprint_string "defineEdge (gs, "; jprint_int id1; 
    comma (); jprint_int id2; jprint_string ");\n"
  in
  match jnode with
      Jatomic (jid, goal, label) -> print_node parent jid goal label
    | Jedge (jid1, jid2) -> print_edge jid1 jid2
    | Jbox (jid, goal, label, g) ->
        (print_node parent jid goal label;
         print_jgraph hyps_and_goal (Some jid) g)
and print_jgraph hyps_and_goal parent jgraph = 
  List.iter (print_jnode hyps_and_goal parent) jgraph;;

let print_defgraph hyps_and_goal name jgraph =
  jprint_string "function defgraph() {\n";
  jprint_string jindent; jprint_string "gs = createGraphStruct();\n"; 
  print_jgraph hyps_and_goal None jgraph;
  jprint_string jindent; jprint_string "return {name:"; 
  jprint_fstring name; jprint_string ", gs: gs};\n";
  jprint_string "}\n";;

end;;



  
     
  
   
