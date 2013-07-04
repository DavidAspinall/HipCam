(* ========================================================================== *)
(* jgraph.ml                                                                  *)
(* Exporting hiproofs to javascript visualisation.                            *)
(*                                                                            *)
(* by Steven Obua                                                             *)
(* Copyright (c) University of Edinburgh, 2012                                *)
(* Copyright (c) Steven Obua, 2012                                            *)
(* ========================================================================== *)

module Neograph = struct

  open Neoprinter;;
    
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

(* Property syntax: "{n:v,...}" *)
let print_props props = (* string properties *)
  (let ncolonv (n,v) = n^":'"^v^"'" in
   neoprint_string " {";
   neoprint_seplist ncolonv ", " props;
   neoprint_string "}")

(* Node syntax: "(n {props})" *)
let print_node name props = 
  (neoprint_string "    (";
   neoprint_string name;
   if (props!=[]) then print_props props;
   neoprint_string ")")

(* Arrow syntax: "(n1-[NAME:]->n2)" or "(n1-[NAME: {props}]->n2)" *)
let print_arrow n1 reln n2 props =
  (neoprint_string ("    (" ^ n1 ^ "-[" ^ (String.uppercase reln) ^":");
   if (props!=[]) then print_props props;
   neoprint_string ("]->" ^ n2 ^ ")"))

let root_name = "root"
let node_name id = "node" ^ (string_of_int id)

(* NB: seems likely to output hyps/goal many times *)
let rec print_graph_node hyps_and_goal parent jnode =
  let print_node1 parent jid goal label =
    let (hyps, goal) = hyps_and_goal goal in
    let parentname = (match parent with None -> root_name | Some jid -> node_name jid) in
    let nodename = node_name jid in
      print_node nodename
	 [("hyps", "["^(seplist ", " hyps)^"]"); (* FIXME: should be escaped hyps *)
	  ("goal", String.escaped goal);
	  ("label", neostring_of_label label)];
       neoprint_string ",\n"; (* flush? *)
       print_arrow parentname "contains" nodename []
  in
  match jnode with
      Jatomic (jid, goal, label) -> print_node1 parent jid goal label
    | Jedge (jid1, jid2) -> print_arrow (node_name jid1) "then" (node_name jid2) []
    | Jbox (jid, goal, label, g) ->
        (print_node1 parent jid goal label;
	 neoprint_string ",\n"; (* flush? *)
         print_graph hyps_and_goal (Some jid) g);
and print_graph hyps_and_goal parent jgraph = 
 neoprint_seplist (print_graph_node hyps_and_goal parent) ",\n" (* flush? *) jgraph;; 

let neoprint_create hyps_and_goal name jgraph =
  neoprint_string "CREATE ";
  print_graph hyps_and_goal None jgraph;
  neoprint_string ";\n";;

end;;



  
     
  
   
