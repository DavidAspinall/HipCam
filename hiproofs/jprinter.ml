module Jprinter = struct

  (* da: is this buffer actually used as such?  I can't see any nested calls to start *)
  let jprint_buffer = ref ([] : string list);;
  
  let jprint_start () = (jprint_buffer := (""::!jprint_buffer));;
  let jprint_stop () = 
    let s = hd (!jprint_buffer) in 
    (jprint_buffer := tl (!jprint_buffer); s);;  
  let jprint_string s = 
    (jprint_buffer := (((hd (!jprint_buffer))^s)::(tl (!jprint_buffer))));;
  let jprint_int i = jprint_string (string_of_int i);;
  
  (* option *)
  
  let jprint_option x0 x_ =
    match x_ with
        Some x -> jprint_string x
      | None   -> jprint_string x0;;
  
  (* seplists *)
  
  let jprint_seplist p sep xs =
    if (xs = [])
    then ()
    else (p (hd xs);
          do_list (fun x -> jprint_string sep; p x) (tl xs));;
  
  let jprint_string_seplist sep xs =
    jprint_seplist jprint_string sep xs;;
  
  
  let jprint_indent i = jprint_string (String.make i ' ');;
  
  let jprint_string_if b x = if b then jprint_string x;;

  (* da NB: String.escaped follows OCaml conventions, not Javascript/bash *)
  let jprint_fstring x = jprint_string ("\"" ^ String.escaped x ^ "\"");;
  
  let jprint_fterm tm =
    let s =        
      if (is_var tm)
      then let (x,ty) = dest_var tm in
           x ^ ":" ^ string_of_type ty
      else string_of_term tm 
    in
    (jprint_string "`";
     jprint_string s;
     jprint_string "`");;
  
  let jprint_ftype ty =
    (jprint_string "`";
     jprint_string (string_of_type ty);
     jprint_string "`");;
  
  let jprint_indent d = jprint_string (String.make d ' ');;
  
  let jprint_goalid id = jprint_int id;;
  
  let rec jprint_label l =
    match Hiproofs.dest_important_label l with
        None -> 
          (match Hiproofs.dest_string_label l with
              None -> 
                (match dest_rich_label l with
                    None -> jprint_fstring "unknown"
                  | Some (l, _, _, []) -> jprint_fstring l
                  | Some (l, _, _, _) -> jprint_fstring (l^" *"))
            | Some x -> jprint_fstring x)
      | Some l -> jprint_label l;;

  let rec get_label l =
    match Hiproofs.dest_important_label l with
        None -> 
          (match Hiproofs.dest_string_label l with
              None -> 
                (match dest_rich_label l with
                    None -> "unknown"
                  | Some (l, _, _, []) -> l
                  | Some (l, _, _, _) -> (l^" *"))
            | Some x -> x)
      | Some l -> get_label l;;

  let rec get_highest_label hi = 
    match (hiproof hi) with
    | Hi_label (label,_,_,_) -> label
    | Hi_atomic (_,label,_,_) -> label
    | _ -> string_label ("`"^(string_of_term (snd (dest_thm hi)))^"`");;

  let get_rich_label l = 
    let name_rich rich = List.map (fun s -> get_highest_label s) rich in
    let list_terms terms =
      match terms with
      | [] -> None
      | _ -> Some (String.concat "; " (List.map (fun s -> "`"^(string_of_term s)^"`") terms))
    in let list_thms thms = 
      match thms with
      | [] -> None
      | _ -> Some (String.concat "; " (List.map (get_label) (name_rich thms)))
    in match Hiproofs.dest_important_label l with
          None -> 
            (match Hiproofs.dest_string_label l with
                None -> 
                  (match dest_rich_label l with
                      None -> (Some "unknown", None, None)
                    | Some (l, _, terms, thms) -> (Some l, list_terms terms, list_thms thms))
              | Some x -> (Some x, None, None))
        | Some l -> (Some (get_label l), None, None);;            

  let jprint_richlabel label = 
    let (_, terms, thms) = get_rich_label label in
    let text = 
      match (terms,thms) with
      | (None, None) -> "{}"
      | (Some tr, Some th) -> "{terms:\"("^(String.escaped tr)^")\", thms: \"["^(String.escaped th)^"]\"}"
      | (Some tr, None) -> "{terms:\"("^(String.escaped tr)^")\"}"
      | (None, Some th) -> "{terms:\"["^(String.escaped th)^"]\"}"
    in jprint_string text;;
  
end
  
