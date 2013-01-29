(* tactics.ml *)
let DISCH_TAC = hilabel_tac_str "DISCH_TAC" DISCH_TAC;;
let EQ_TAC =   hilabel_tac_split "Equivalence" "==>" "<==" EQ_TAC;; 
let GEN_TAC = hilabel_tac_str "GEN_TAC" GEN_TAC;;
let EXISTS_TAC = hilabel_tac_p (string_label "EXISTS_TAC") EXISTS_TAC;;
let CONJ_TAC = hilabel_tac_str "CONJ_TAC" CONJ_TAC;;
let MATCH_MP_TAC = hilabel_tac_p (string_label "MATCH_MP_TAC") MATCH_MP_TAC;;
let STRIP_TAC = hilabel_tac_str "STRIP_TAC" STRIP_TAC;;

let nprove n x = store_thm (Some n) (prove x);;
let prove x = store_thm None (prove x);;
