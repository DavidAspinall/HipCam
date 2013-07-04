(* ========================================================================== *)
(* neograph.ml                                                                *)
(* Exporting hiproofs to javascript visualisation.                            *)
(*                                                                            *)
(* by Steven Obua                                                             *)
(* Copyright (c) University of Edinburgh, 2012                                *)
(* Copyright (c) Steven Obua, 2012                                            *)
(* ========================================================================== *)

module Neograph = struct

  open Jgraph;;      (* reuse datatypes from there *)
  open Neoprinter;;
    
(* Property syntax: "{n:v,...}" *)
let print_props props = (* string properties *)
  (let ncolonv (n,v) = neoprint_string (n^":\'"^v^"\'") in
   neoprint_string " {";
   neoprint_seplist ncolonv ", " props;
   neoprint_string "}")

(* Node syntax: "(n {props})" *)
let print_node name props = 
  (neoprint_string "    (";
   neoprint_string name;
   if (props!=[]) then print_props props;
   neoprint_string ")")

(* Arrow syntax: "(n1)-[:NAME]->(n2)" or "(n1)-[:NAME {props}]->(n2)" *)
let print_arrow n1 reln n2 props =
  (neoprint_string ("    (" ^ n1 ^ ")-[:" ^ (String.uppercase reln));
   if (props!=[]) then print_props props;
   neoprint_string ("]->(" ^ n2 ^ ")"))


(* NB: seems likely to output hyps/goal many times *)
let rec print_graph_node hyps_and_goal parent jnode =
  let root_name = "root" in
  let node_name id = "node" ^ (string_of_int id) in
  let print_node1 parent jid goal label =
    let (hyps, goal) = hyps_and_goal goal in
    let parentname = (match parent with None -> root_name | Some jid -> node_name jid) in
    let nodename = node_name jid in
      print_node nodename
	 [("hyps", "["^(seplist ", " (map String.escaped hyps))^"]");
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
 neoprint_seplist (print_graph_node hyps_and_goal parent) ",\n" (* flush? *) jgraph

let neoprint_create hyps_and_goal name jgraph =
  neoprint_string "CREATE ";
  print_graph hyps_and_goal None jgraph;
  neoprint_string ";\n";;

end;;



  
     
  
   
