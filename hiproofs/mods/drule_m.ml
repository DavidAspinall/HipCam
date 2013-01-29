(* drule.ml *)
let BETAS_CONV tm = hilabel_thm false (simple_label "BETAS_CONV") (BETAS_CONV tm);;
let INSTANTIATE inst = hilabel_thm_thm (simple_label "INSTANTIATE") (INSTANTIATE inst);;
let INSTANTIATE_ALL inst = hilabel_thm_thm (simple_label "INSTANTIATE_ALL") (INSTANTIATE_ALL inst);;
