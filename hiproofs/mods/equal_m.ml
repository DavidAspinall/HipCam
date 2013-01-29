(* equal.ml *)
let BETA_CONV tm = hilabel_thm false (simple_label "BETA_CONV") (BETA_CONV tm);;
let AP_TERM tm = hilabel_thm_thm (simple_label "AP_TERM") (AP_TERM tm);;
let AP_THM th tm = hilabel_thm_thm (simple_label "AP_THM") (fun th -> AP_THM th tm) th;;
let SYM = hilabel_thm_thm (simple_label "SYM") SYM;;
let ALPHA tm1 tm2 = hilabel_thm false (simple_label "ALPHA") (ALPHA tm1 tm2);;
