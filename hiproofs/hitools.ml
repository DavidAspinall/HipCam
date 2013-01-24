module Hitools = struct

open Hiproofs;;
open Jgraph;;

let rec hishrink hi =
  match hi with 
      Hi_label (l, g, hi, _) ->
        (match hisize hi with
            Too_complex -> 
              label l (hishrink' hi)
          | Complexity 1 ->
              hishrink hi
          | _ -> label l (hishrink hi))
    | Hi_tensor (his, _) ->
        tensor (List.map hishrink his)
    | Hi_sequence (his, _) ->
        sequence (List.map hishrink his)
    | _ -> hi
and hishrink' hi =
  match hi with 
      Hi_tensor (his, _) ->
        tensor (List.map hishrink'' his)
    | Hi_sequence (his, _) ->
        sequence (List.map hishrink'' his)
    | _ -> hishrink hi
and hishrink'' hi =
  match hisize hi with
      Too_complex -> 
        tensor (List.map 
                  (fun hi -> atomic (arity_out hi) (make_string_label "too big") (Hiproofs.input hi)) 
                  (forest hi))
    | _ -> hishrink hi;;

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


let hiexport hi name =
  Jexport.jexport 
    (fun (hs, c) -> (List.map string_of_term hs, string_of_term c))
    name 
    (jgraph_of_hiproof (hishrink hi));;

let export th name = hiexport (hiproof th) name;;
            


end
          
 
      
      

