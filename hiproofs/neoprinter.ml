module Neoprinter = struct

  (* da: is this buffer actually used as such?  I can't see any nested calls to start,
  just seems to append to first element to build a really huge string all the time? *)
  let neoprint_buffer = ref ([] : string list);;
  
  let neoprint_start () = (neoprint_buffer := (""::!neoprint_buffer));;
  let neoprint_stop () = 
    let s = hd (!neoprint_buffer) in 
    (neoprint_buffer := tl (!neoprint_buffer); s);;  
  let neoprint_string s = 
    (neoprint_buffer := (((hd (!neoprint_buffer))^s)::(tl (!neoprint_buffer))));;
  let neoprint_int i = neoprint_string (string_of_int i);;
  
  (* option *)
  
  let neoprint_option x0 x_ =
    match x_ with
        Some x -> neoprint_string x
      | None   -> neoprint_string x0;;
  
  (* seplists *)
  
  let neoprint_seplist p sep xs =
    if (xs = [])
    then ()
    else (p (hd xs);
          do_list (fun x -> neoprint_string sep; p x) (tl xs));;

  let neoprint_string_seplist sep xs =
    neoprint_seplist neoprint_string sep xs;;

  (* FIXME: clean this up *)
  let rec seplist sep xs =
    if (xs = [])
    then ""
    else (hd xs) ^ sep ^ (seplist sep (tl xs));;
  
  let neoprint_indent i = neoprint_string (String.make i ' ');;
  
  let neoprint_string_if b x = if b then neoprint_string x;;

  (* da NB: String.escaped follows OCaml conventions, not Javascript/bash *)
  let neoprint_fstring x = neoprint_string ("\"" ^ String.escaped x ^ "\"");;
  
  let neoprint_fterm tm =
    let s =        
      if (is_var tm)
      then let (x,ty) = dest_var tm in
           x ^ ":" ^ string_of_type ty
      else string_of_term tm 
    in
    (neoprint_string "`";
     neoprint_string s;
     neoprint_string "`");;
  
  let neoprint_ftype ty =
    (neoprint_string "`";
     neoprint_string (string_of_type ty);
     neoprint_string "`");;
  
  let neoprint_indent d = neoprint_string (String.make d ' ');;
  
  let neoprint_goalid id = neoprint_int id;;
  
  let rec neoprint_label l =
    match Hiproofs.dest_important_label l with
        None -> 
          (match Hiproofs.dest_string_label l with
              None -> 
                (match dest_rich_label l with
                    None -> neoprint_fstring "unknown"
                  | Some (l, _, _, []) -> neoprint_fstring l
                  | Some (l, _, _, _) -> neoprint_fstring (l^" *"))
            | Some x -> neoprint_fstring x)
      | Some l -> neoprint_label l;;
            
  let rec neostring_of_label l =
    match Hiproofs.dest_important_label l with
        None -> 
          (match Hiproofs.dest_string_label l with
              None -> 
                (match dest_rich_label l with
                    None -> "unknown"
                  | Some (l, _, _, []) -> l
                  | Some (l, _, _, _) -> (l^" *"))
            | Some x -> x)
      | Some l -> neostring_of_label l;;
  
end
  
