module Hiproofs : sig

  type label
  type 'a labelconstr

  val labelconstr : unit -> 'a labelconstr
  val make_label : 'a labelconstr -> 'a -> label
  val dest_label : 'a labelconstr -> label -> 'a option

  val lc_string : string labelconstr
  val make_string_label : string -> label
  val dest_string_label : label -> string option

  type complexity = Too_complex | Complexity of int

  type varname = int
  type varset = varname list
  type 'a varmap = (varname * 'a) list
  
  type 'a info = int * int * ('a varmap) * complexity

  (* 
     A normalized hiproof obeys the following constraints:
     1) no direct child of a tensor is a tensor
     2) no direct child of a sequence is a sequence
     3) there are no sequences or tensors that have exactly 1 child
     4) no direct child of a sequence is the identity or a tensor of identities
     5) there is no empty tensor
     6) there is no empty sequence

     You can ensure that all hiproofs are normalized by only using the functions
     sequence, tensor and seq_tensor for forming sequences and tensors.
  *)

  type 'a hiproof = 
      Hi_atomic of int * label * 'a * ('a varmap)
    | Hi_id of 'a
    | Hi_cut of 'a
    | Hi_var of varname * 'a
    | Hi_label of label * 'a * 'a hiproof * 'a info
    | Hi_tensor of 'a hiproof list * 'a info
    | Hi_sequence of 'a hiproof list * 'a info;;

  val hisize : 'a hiproof -> complexity
  val hicomplexity : varset -> 'a hiproof -> complexity 

  val arity_in : 'a hiproof -> int
  val arity_out : 'a hiproof -> int
  
  val tensor : 'a hiproof list -> 'a hiproof
  val sequence : 'a hiproof list -> 'a hiproof
  val seq_tensor : 'a hiproof -> 'a hiproof list -> 'a hiproof
  val label : label -> 'a hiproof -> 'a hiproof
  val atomic : int -> label -> 'a -> 'a hiproof
  val info_of : 'a hiproof -> 'a info

  val forest : 'a hiproof -> 'a hiproof list
  
  val turn_vars : varset -> 'a hiproof ->'a varmap * 'a hiproof 
  val alloc_vars : int -> varname
  val release_vars : varname -> int -> unit

  type 'thm rule = ('thm list -> 'thm) 

  val label_rule : 
      ('thm -> 'a hiproof) * ('a hiproof -> 'thm -> 'thm) 
      -> label -> 'thm rule -> 'thm rule 

  val calcsize : 'a hiproof -> int

  val shrinksize : int -> 'a hiproof -> int

  val children : 'a hiproof -> 'a hiproof list

  val input : 'a hiproof -> 'a

(* Utility functions *)
    
  val replicate : 'a -> int -> 'a list

  val split : int -> 'a list -> ('a list) * ('a list)
 
end = struct

  module Univ = struct
    type t = exn
        
    type 'a embed = { put_in : 'a -> t ; get_out : t -> 'a option }
        
    let create (type s) () = begin 
      let module M = struct
        exception E of s
      end in
      { put_in = (fun x -> M.E x) ;
        get_out = (function M.E x -> Some x | _ -> None)
      }  
    end
      
    let box e a = e.put_in a
    let unbox e t = e.get_out t
  end

  type label = Univ.t
  type 'a labelconstr = 'a Univ.embed

  let labelconstr () = Univ.create ();;
  let make_label lc content = Univ.box lc content;;
  let dest_label lc label = Univ.unbox lc label;; 

  let lc_string = labelconstr ();;

  let make_string_label = make_label lc_string
  let dest_string_label = dest_label lc_string

  type complexity = Too_complex | Complexity of int

  type varname = int
  type varset = varname list
  type 'a varmap = (varname * 'a) list
  
  type 'a info = int * int * ('a varmap) * complexity

  type 'a hiproof = 
      Hi_atomic of int * label * 'a * ('a varmap)
    | Hi_id of 'a
    | Hi_cut of 'a
    | Hi_var of varname * 'a
    | Hi_label of label * 'a * 'a hiproof * 'a info
    | Hi_tensor of 'a hiproof list * 'a info
    | Hi_sequence of 'a hiproof list * 'a info;;

  let rec next_free_var = ref 0;;

  let alloc_vars n = 
    let x = !next_free_var in
    let _ = (next_free_var := x + n) in
    x;;

  let release_vars x n = 
    if (!next_free_var) = x+n then
      next_free_var := x
    else
      ();;
  
  let info_of hi =
    match hi with
        Hi_atomic (n, _, _, vs) -> (1, n, vs, Complexity 1)
      | Hi_id _ -> (1, 1, [], Complexity 1)
      | Hi_cut _ -> (1, 0, [], Complexity 1)
      | Hi_var (v,g) -> (1, 0, [(v,g)], Complexity 1)
      | Hi_label (_, _, _, info) -> info
      | Hi_tensor (_, info) -> info
      | Hi_sequence (_, info) -> info;;

  let rec merge_varmaps xs ys =
    match (xs, ys) with
        ((x,gx)::xs', (y,gy)::ys') ->
          if x < y then (x,gx) :: (merge_varmaps xs' ys)
          else if y < x then (y,gy) :: (merge_varmaps xs ys')
          else (x,gx) :: (merge_varmaps xs' ys')
      | (xs, []) -> xs
      | ([], ys) -> ys;;

  let rec merge_vars xs ys =
    match (xs, ys) with
        (x::xs', (y,_)::ys') ->
          if x < y then x :: (merge_vars xs' ys)
          else if y < x then y :: (merge_vars xs ys')
          else x :: (merge_vars xs' ys')
      | (xs, []) -> xs
      | ([], ys) -> (List.map fst ys);;
  
  let complexity_treshold = 100;;

  let add_complexity c1 c2 =
    match (c1, c2) with 
        (Complexity c1, Complexity c2) ->
          if c1 + c2 < complexity_treshold then 
            Complexity (c1 + c2)
          else
            Too_complex
      | _ -> Too_complex;;

  let inc_info (a_in, a_out, v, c) = (a_in, a_out, v, add_complexity c (Complexity 1));;

  let tensor = 
    let add_info (in1, out1, vars1, size1) (in2, out2, vars2, size2) =
      (in1 + in2, out1 + out2, merge_varmaps vars1 vars2, add_complexity size1 size2)
    in
    let rec mk his =
      match his with
          [] -> ([], (0, 0, [], Complexity 0))
        | (hi::his) -> 
            (let (his, info) = mk his in
             match hi with
                 Hi_tensor (sub_his, sub_info) -> 
                   (sub_his@his, add_info sub_info info)
               | _ -> (hi::his, add_info info (info_of hi)))
    in
    fun his -> 
      match mk his with
          ([], _) -> failwith "there is no empty tensor"
        | ([hi], _) -> hi
        | (his, info) -> Hi_tensor (his, inc_info info);; 
  
  let sequence =
    let add_info (in1, out1, vars1, size1) (in2, out2, vars2, size2) =
      if size2 = Complexity 0 then
        (in1, out1, vars1, size1)
      else
        if out1 = in2 then
          (in1, out2, merge_varmaps vars1 vars2, add_complexity size1 size2)
        else
          failwith ("different arities of outputs and inputs in sequence: " ^ 
                       (string_of_int out1) ^ "/" ^ (string_of_int in2))
    in
    let rec is_identity hi =
      match hi with
          Hi_id _ -> true
        | Hi_tensor (his, _) -> List.for_all is_identity his
        | _ -> false
    in
    let rec mk his =
      match his with
          [] -> (None, [], (0, 0, [], Complexity 0))
        | (hi::his) -> 
            (let (id, his, info) = mk his in
             match hi with
                 Hi_sequence (sub_his, sub_info) ->
                   (None, sub_his@his, add_info sub_info info)
               | _ -> 
                   if is_identity hi then
                     (Some hi, his, info)
                   else 
                     (None, hi::his, add_info (info_of hi) info))
    in
    fun his ->
      match mk his with
          (None, [], _) -> failwith "there is no empty sequence"  
        | (Some hi, [], _) -> hi
        | (_, [hi], _) -> hi
        | (_, his, info) -> Hi_sequence (his, inc_info info);;  
  
  let seq_tensor hi his =
    match his with
        [] -> hi
      | _ -> sequence [hi; tensor his];;
  

  let atomic n l g = Hi_atomic (n, l, g, [])

  let rec inputs hi =
    match hi with
        Hi_atomic (_, _, g, _) -> [g]
      | Hi_id g -> [g]
      | Hi_cut g -> [g]
      | Hi_var (_, g) -> [g]
      | Hi_label (_, g, hi, _) -> [g]
      | Hi_tensor (his, _) -> List.concat (List.map inputs his)
      | Hi_sequence (his, _) -> inputs (List.hd his);;

  let input hi = 
    match inputs hi with
        [hi] -> hi
      | _ -> failwith "no or more than one inputs";; 

  let label l hi = 
    match info_of hi with
        (ain, aout, vars, _) ->
          Hi_label (l, input hi, hi, (ain, aout, vars, Complexity 1));;

  let sum xs = List.fold_left (fun a b -> a + b) 0 xs;;

  let arity_in hi = match info_of hi with (n, _, _, _) -> n;;
  let arity_out hi = match info_of hi with (_, n, _, _) -> n;;
  let hisize hi = match info_of hi with (_, _, _, s) -> s;;
  let hivars hi = match info_of hi with (_, _, v, _) -> v;;

  let rec disjoint_vars us vs =
    match (us, vs) with
        (u::us', (v, _)::vs') -> 
          if u < v then
            disjoint_vars us' vs 
          else if v < u then
            disjoint_vars us vs'
          else false
      | _ -> true;;

  let hicomplexity vars =
    let rec hic c hi =
      let lower_bound = add_complexity c (hisize hi) in
      match lower_bound with
          Too_complex -> Too_complex
        | _ ->
            if disjoint_vars vars (hivars hi) then
              lower_bound
            else
              let c = add_complexity c (Complexity 1) in
              match hi with
                  Hi_var _ 
                | Hi_atomic _ -> c
                | Hi_label (_, _, hi, _) -> hic c hi
                | Hi_tensor (his, _) 
                | Hi_sequence (his, _) -> List.fold_left hic c his
                | _ -> failwith "hicomplexity: internal error"
    in
    hic (Complexity 0);;

  let rec calcsize hi =
    match hi with
        Hi_label (_, _, hi, _) -> 1 + (calcsize hi)
      | Hi_sequence (his, _) 
      | Hi_tensor (his, _) -> 1 + (sum (List.map calcsize his))
      | _ -> 1;;


  let rec shrinksize max hi =
    match hi with
        Hi_label (_, _, hi, _) ->
          (match hisize hi with
              Too_complex -> 1
            | Complexity c ->
                if c <= max then 1 + (shrinksize max hi) else 1)
      | Hi_sequence (his, _) 
      | Hi_tensor (his, _) -> 1 + (sum (List.map (shrinksize max) his))
      | _ -> 1;;
  
    
  let rec split n xs =
    match xs with
        [] -> ([], [])
      | (x::xs') -> 
          if n <= 0 then 
            ([], xs) 
          else 
            let (h,t) = split (n-1) xs' in
            (x::h, t);;

  let forest : 'a hiproof -> 'a hiproof list =
    let rec group forest1 forest2 =
      match forest1 with
          [] -> []
        | (hi::forest1) -> 
            let (his, forest2) = split (arity_out hi) forest2 in
            (seq_tensor hi his)::(group forest1 forest2) 
    in      
    let rec forest' hi = 
      match hi with
          Hi_tensor (his, _) -> 
            List.concat (List.map forest' his)
        | Hi_sequence (his, _) -> 
            if (arity_in (List.hd his) = 1) then 
              [hi]
            else
              group (forest' (List.hd his)) (forest' (sequence (List.tl his)))
        | _ -> [hi]
    in
    forest';;

  let rec replicate x n = if n <= 0 then [] else x::(replicate x (n-1));; 

  let print_vars s vs = print_endline (String.concat "," (s :: (List.map (fun v -> (string_of_int v) ^ " ") vs)));;

  let string_of_complexity c =
    match c with
        Too_complex -> "TOO COMPLEX"
      | Complexity c -> "COMPLEXITY "^(string_of_int c);;

  let turn_vars turning_vars =
    let rec combine vars outputs his =
      (match (outputs, his) with
          ([], []) ->  ([], [])
        | ((Some (v,g))::outputs, his) -> 
            let (outputs, his) = combine vars outputs his in
            ((Some (v,g))::outputs, (Hi_id g)::his)
        | (None::outputs, hi::his) ->
            let (outputs_hi, hi') = turn vars hi in
            let (outputs', his') = combine (merge_vars vars (hivars hi)) outputs his in
            (outputs_hi@outputs', hi'::his')
        | _ -> failwith "turn_vars/combine: internal error")
    and turn vars hi =
      if disjoint_vars turning_vars (hivars hi) then
        (replicate None (arity_out hi), hi)
      else
        match hi with
            Hi_var (v, g) -> 
              if List.mem v vars then 
                ([], Hi_cut g) 
              else
                ([Some (v,g)], Hi_id g)
          | Hi_atomic (n, l, g, vs) ->
              let vs = List.filter (fun (v,_) -> not (List.mem v vars)) vs in
              let outputs = 
                (List.map (fun x -> Some x)
                   (List.filter (fun (v,_) -> List.mem v turning_vars) vs))@
                  (replicate None n) in
              let vs = List.filter (fun (v,_) -> not (List.mem v turning_vars)) vs in
              (outputs, Hi_atomic (List.length outputs, l, g, vs))
          | Hi_label (l, g, hi, _) ->              
              if hicomplexity turning_vars hi = Too_complex then
                turn vars (Hi_atomic (arity_out hi, l, g, hivars hi))
              else
                let (outputs, hi) = turn vars hi in
                (outputs, label l hi)            
          | Hi_tensor (his, _) ->
              let (_, outputs, his) = 
                List.fold_right
                  (fun hi (vars, outputs, his)  ->
                    let (outputs', hi') = turn vars hi in
                    (merge_vars vars (hivars hi), outputs'@outputs, hi'::his))
                  his (vars, [], [])
              in
              (outputs, tensor his)
          | Hi_sequence (his, _) -> 
              let (outputs, hi) = turn vars (List.hd his) in
              let vars = merge_vars vars (hivars (List.hd his)) in
              let his = forest (sequence (List.tl his)) in 
              let (outputs, his) = combine vars outputs his in
              (outputs, seq_tensor hi his)
          | _ -> failwith "internal error, no vars possible here"           
    in
    fun hi -> 
      let (outputs, hi) = turn [] hi in
      let result = (List.map (function (Some g) -> g) outputs, hi) in
      result;;
  
  type 'thm rule = ('thm list -> 'thm) 
      
  let label_rule (get, set) lb rule ths =
    let n = List.length ths in
    let x = alloc_vars n in
    try 
      let (vars, substitution, ths) = 
        List.fold_right 
          (fun th (vars, s, ths) ->
            let i = List.hd vars in
            let th' =
              set (Hi_var (i,input (get th))) th
            in
            ((i+1)::vars, (i, get th)::s, th'::ths)) ths ([x], [], []) in
      let vars = List.rev (List.tl vars) in
      let th = rule ths in
      let (vars, hi) = turn_vars vars (label lb (get th)) in
      let his = List.map (fun (v,_) -> List.assoc v substitution) vars in
      let result = set (seq_tensor hi his) th in
      let _ = release_vars x n in 
      result
    with ex ->
      release_vars x n;
      raise ex;;

  let children hi = 
    match hi with
        Hi_label (_, _, hi, _) -> [hi]
      | Hi_sequence (his, _) -> his
      | Hi_tensor (his, _) -> his
      | _ -> [];;



end
        
