module Hitools = struct

open Hiproofs;;
open Jgraph;;

let too_complex hi = 
  match hisize hi with
      Too_complex -> true
    | Complexity c -> c > 50;;

let labelstr l = 
  match dest_string_label l with
      None -> "?"
    | Some l -> l;;

let string_of_complexity c =
  match c with
      Too_complex -> "too complex"
    | Complexity c -> "complexity="^(string_of_int c);;

let hiprint hi = 
  let rec pr indent hi =
    let p s = print_endline (indent^s) in
    let u = "size = "^(string_of_complexity (hisize hi)) in
    match hi with
        Hi_label (l, g, _, _) -> 
          p ("label = " ^ (labelstr l) ^", "^u);
          List.iter (fun s -> pr (indent^"  ") s) (Hiproofs.children hi)
      | Hi_tensor _ -> 
          p ("tensor, "^u);
          List.iter (fun s -> pr (indent^"  ") s) (Hiproofs.children hi)
      | Hi_sequence _ -> 
          p ("sequence, "^u);
          List.iter (fun s -> pr (indent^"  ") s) (Hiproofs.children hi)
      | Hi_atomic (n, l, _, _) -> p ("atom "^(labelstr l))
      | Hi_var _ -> p "var"
      | Hi_id _ -> p "id"
      | Hi_cut _ -> p "cut"
  in
  pr "" hi;;

let rec clean outs hi =
  let ins = inputs hi in
  if ins = outs then 
    tensor (List.map (fun g -> Hi_id g) ins)
  else
    match hi with
        Hi_label (l, g, hi, _) ->
          label l (clean outs hi)
      | Hi_tensor (his, _) ->
          let (_, his) = 
            List.fold_left 
              (fun (outs, his) hi ->
                let n = arity_out hi in
                let (outs', outs'') = split n outs in
                let hi' = clean outs' hi in
                (outs'', hi' :: his))
              (outs, [])
              his 
          in
          tensor (List.rev his)
      | Hi_sequence (his, _) -> 
          let (_, his) =
            List.fold_right
              (fun hi (outs, his) ->
                let hi' = clean outs hi in
                (inputs hi', hi'::his))
              his (outs, [])
          in
          sequence his
      | _ -> hi;;
            
let shrinkit shrink total hi = 
  match hisize hi with 
      Too_complex ->
        atomic (arity_out hi) (make_string_label "too big") (input hi)   
    | Complexity n when n > total ->
        atomic (arity_out hi) (make_string_label "too big") (input hi)
    | Complexity n -> shrink (total/n) hi;;


let supershrink =
  let rec shrink total hi = 
    match hi with
        Hi_label (l, g, hi, _) ->
          (match hisize hi with
              Complexity 1 ->
                (match dest_important_label l with
                    None -> shrink total hi 
                  | Some _ -> label l (shrink (total-1) hi))                      
            | Complexity n -> 
                if n > total then 
                  atomic (arity_out hi) l g
              else 
                label l (shrink ((total-n)/n) hi)
            | Too_complex ->
                atomic (arity_out hi) l g)
      | Hi_tensor (his, _) ->
          tensor (List.map (shrink total) his)
      | Hi_sequence (his, _) ->
          sequence (List.map (shrink total) his)
      | _ -> hi
  in
  fun total hi ->
    let hi = shrinkit shrink total hi in
    let hi = clean [] hi in
    shrinkit shrink total hi;;

type shrinksize = Shrink_ok of int | Shrink_cutoff of int;;

let shrink total =
  let compute_usage sizes =
    List.fold_left 
      (fun (ok, cutoff, used) s ->
        match s with
            (Shrink_cutoff n) -> (ok, cutoff+1, used)
          | (Shrink_ok n) -> (ok+1, cutoff, used + n))
      (0, 0, 0) sizes
  in 
  let compute_size sizes = 
    List.fold_left 
      (fun total s -> 
        match s with
            (Shrink_cutoff n) 
          | (Shrink_ok n) -> total+n)
      0 sizes
  in
  let rec shr total hi = 
    match hi with
        Hi_label (l, g, hi, _) ->
          (match hisize hi with
              Complexity n when n+1 > total ->
                ([Shrink_cutoff 1], atomic (arity_out hi) l g)
            | Too_complex ->
                ([Shrink_ok 1], atomic (arity_out hi) l g)
            | Complexity n ->
                let (sizes, hi) = shr ((total-1)/n) hi in
                let rec improve sizes (ok, cutoff, used) hi =                  
                  if ok > 0 & cutoff > 0 then
                    let (sizes, hi) = shr' ((total-1-used)/cutoff) sizes hi in
                    let (ok', cutoff', used') = compute_usage sizes in
                    print_endline ("improve, ok="^(string_of_int ok)^", ok'="^(string_of_int ok'));
                    if ok' > ok then 
                      improve sizes (ok', cutoff', used') hi
                    else
                      (cutoff' = 0, compute_size sizes, hi)
                  else
                    (cutoff = 0, compute_size sizes, hi)
                in
                let (ok, s, hi) = improve sizes (compute_usage sizes) hi in
                let hi = label l hi in
                if ok then ([Shrink_ok (s+1)], hi) else ([Shrink_cutoff (s+1)], hi))
      | Hi_tensor (his, _) ->
          let results = List.map (shr total) his in
          let sizes = List.concat (List.map fst results) in
          let hi = tensor (List.map snd results) in
          (sizes, hi)
      | Hi_sequence (his, _) ->
          let results = List.map (shr total) his in
          let sizes = List.concat (List.map fst results) in
          let hi = sequence (List.map snd results) in
          (sizes, hi)
      | _ -> ([Shrink_ok 1], hi)
  and shr' total sizes hi =
    let f (sizes, acc_sizes, his) hi = 
      let (s, h) = shr' total sizes hi in
      let n = List.length s in
      let (_, sizes') = split n sizes in
      (sizes', acc_sizes@s, h::his)
    in
    match hi with
        Hi_tensor (his, _) ->         
          let (_, sizes, his) = List.fold_left f (sizes, [], []) his in
          (sizes, tensor (List.rev his))
      | Hi_sequence (his, _) ->
          let (_, sizes, his) = List.fold_left f (sizes, [], []) his in
          (sizes, sequence (List.rev his))
      | _ -> 
          (match List.hd sizes with
              Shrink_cutoff n  ->
                shr total hi
            | s ->
                ([s], hi))
  in
  shrinkit (fun total hi -> snd (shr total hi)) total;;


let jgraph_of_hiproof' hi =
  let id_counter = ref 0 in
  let next_id () = (id_counter := (!id_counter) + 1; !id_counter) in
  let rec replicate n x = if n <= 0 then [] else x::(replicate (n-1) x) in
  let rec jhi hi =
    match hi with
        Hi_atomic (n, l, g, _) -> 
          let id = next_id() in
          ([id], replicate n id, [Jatomic (id, g, l)])
      | Hi_id g -> 
          let id = next_id() in
          ([id], [id], [Jatomic (id, g, make_string_label "ID")])
      | Hi_cut g ->
          let id = next_id() in
          ([id], [], [Jatomic (id, g, make_string_label "CUT")])
      | Hi_var (v, g) ->
          let id = next_id() in
          ([id], [], [Jatomic (id, g, make_string_label ("VAR "^(string_of_int v)))])
      | Hi_label (l, g, hi, _) ->
          let (ins, outs, jg) = jhi hi in
          (ins, outs, [Jbox (next_id(), g, l, jg)])
      | Hi_tensor (his, _) -> 
          let his = List.map jhi his in
          (List.concat (List.map (fun (x,_,_) -> x) his),
           List.concat (List.map (fun (_,x,_) -> x) his),
           List.concat (List.map (fun (_,_,x) -> x) his))
      | Hi_sequence (his, _) ->
          let his = List.map jhi his in
          let edge x y = Jedge (x, y) in
          let join (ins1, outs1, jg1) (ins2, outs2, jg2) =
            (ins1, outs2, jg1@jg2@(List.map2 edge outs1 ins2))
          in
          List.fold_left join (List.hd his) (List.tl his)
  in
  let (_, _, jg) = jhi hi in
  jg;;

let jgraph_of_hiproof hi =
  let id_counter = ref 0 in
  let next_id () = (id_counter := (!id_counter) + 1; !id_counter) in
  let rec jhi inputs hi =
    match hi with
        Hi_atomic (n, l, g, _) -> 
          let id = next_id() in
          ([id], replicate id n, [Jatomic (id, g, l)])
      | Hi_id g -> 
          let id = next_id() in
          ([id], [id], [Jatomic (id, g, make_string_label "ID")])
          (*let id = List.hd inputs in
          ([id], [id], [])*)
      | Hi_cut g ->
          let id = next_id() in
          ([id], [], [Jatomic (id, g, make_string_label "CUT")])
      | Hi_var (v, g) ->
          let id = next_id() in
          ([id], [], [Jatomic (id, g, make_string_label ("VAR "^(string_of_int v)))])
      | Hi_label (l, g, hi, _) ->
          let (ins, outs, jg) = jhi inputs hi in
          (ins, outs, [Jbox (next_id(), g, l, jg)])
      | Hi_tensor (his, _) -> 
          let f (inputs, (ins, outs, jg)) hi =
            let (ins1, ins2) = split (arity_in hi) inputs in
            let (ins', outs', jg') = jhi ins1 hi in
            (ins2, (ins@ins', outs@outs', jg@jg')) 
          in
          snd (List.fold_left f (inputs, ([], [], [])) his)
      | Hi_sequence (his, _) ->
          let edge x y = Jedge (x, y) in
          let g l (x, y) = if x = y then l else (edge x y)::l in
          let f (ins, outs, jg) hi =
            let (ins', outs', jg') = jhi outs hi in
            let edges = List.fold_left g [] (List.combine outs ins') in
            (ins, outs', jg@jg'@edges)
          in
          List.fold_left f (jhi inputs (List.hd his)) (List.tl his)
  in
  let (_, _, jg) = jhi [next_id()] hi in
  jg;;


let hiexport n hi name =
  Jexport.jexport 
    (fun (hs, c) -> (List.map string_of_term hs, string_of_term c))
    name 
    (jgraph_of_hiproof (clean [] (shrink n hi)));;

let export n th name = hiexport n (hiproof th) name;;
            
let info = "β conversion";;


end
          
 
      
      

