open Hiproofs;;

module Hiextract : sig

	val extract : 'a hiproof -> 'a hiproof

end = struct

	let rec get_labels hi =
		match hi with
		| Hi_label(label,_,his,_) -> (
			match his with
			| Hi_tensor(_,_) -> get_labels his
			| _ -> [label])
		| Hi_sequence(his,_) -> get_labels (List.hd his)
		| Hi_atomic(_,label,_,_) -> [label]
		| Hi_tensor(his,_) -> List.flatten (List.map get_labels his)
		| _ -> [];;

	let extract hi =
		let all_inputs = Hiproofs.inputs hi in
		let names = get_labels hi in
		let mk_atomic0 x y = atomic 0 y x in
			tensor (List.map2 mk_atomic0 all_inputs names);;
end
