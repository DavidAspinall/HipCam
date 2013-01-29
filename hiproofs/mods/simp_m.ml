(* simps.ml *)
let REWRITE_CONV thl tm = hilabel_thm false (rich_label ("REWRITE_CONV", [], [], thl)) (REWRITE_CONV thl tm);;
let GEN_REWRITE_TAC cnvl = hilabel_tac_thms "GEN_REWRITE_TAC" (GEN_REWRITE_TAC cnvl);;
let REWRITE_TAC = hilabel_tac_thms "REWRITE_TAC" REWRITE_TAC;;
let ONCE_REWRITE_TAC = hilabel_tac_thms "ONCE_REWRITE_TAC" ONCE_REWRITE_TAC;;
let ASM_REWRITE_TAC = hilabel_tac_thms "ASM_REWRITE_TAC" ASM_REWRITE_TAC;;
let SIMP_TAC = hilabel_tac_thms "SIMP_TAC" SIMP_TAC;;
let ASM_SIMP_TAC = hilabel_tac_thms "ASM_SIMP_TAC" ASM_SIMP_TAC;;
