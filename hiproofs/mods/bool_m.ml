(* bool *)
let TRUTH = hilabel_thm false (simple_label "TRUTH") TRUTH;;

let EQT_ELIM = hilabel_thm_thm (string_label "EQT_ELIM") EQT_ELIM;;
let EQT_INTRO = hilabel_thm_thm (string_label "EQT_INTRO") EQT_INTRO;;

let CONJ = hilabel_thm_split "A and B" "A" "B" CONJ;;
let CONJUNCT1 = hilabel_thm_thm (string_label "CONJUNCT1") CONJUNCT1;;
let CONJUNCT2 = hilabel_thm_thm (string_label "CONJUNCT2") CONJUNCT2;;
let MP = hilabel_thm_split "Modus ponens B" "A ==> B" "A" MP;;

let DISCH tm = hilabel_thm_thm (simple_label "DISCH") (DISCH tm);;
let DISCH_ALL = hilabel_thm_thm (simple_label "DISCH_ALL") DISCH_ALL;;
let UNDISCH = hilabel_thm_thm (simple_label "UNDISCH") UNDISCH;;
let UNDISCH_ALL = hilabel_thm_thm (simple_label "UNDISCH_ALL") UNDISCH_ALL;;
let IMP_ANTISUM_RULE = hilabel_thm_split "IMP_ANTISYM_RULE A B" "A" "B" IMP_ANTISYM_RULE;;
let ADD_ASSUM tm = hilabel_thm_thm (rich_label ("ADD_ASSUM", [], [tm], [])) (ADD_ASSUM tm);;
let EQ_IMP_RULE =
  let f th = fst (EQ_IMP_RULE th) in
  let g th = snd (EQ_IMP_RULE th) in
  let f' = hilabel_thm_thm (string_label "EQ_IMP_RULE1") f in
  let g' = hilabel_thm_thm (string_label "EQ_IMP_RULE2") g in 
  fun th -> (f' th, g' th);;
let IMP_TRANS = hilabel_thm_split "IMP_TRANS A B" "A" "B" IMP_TRANS;;
let SPEC tm = hilabel_thm_thm (rich_label ("SPEC", [], [tm], [])) (SPEC tm);;
let SPEC_ALL = hilabel_thm_thm (simple_label "SPEC_ALL") SPEC_ALL;;
let GEN tm = hilabel_thm_thm (simple_label "GEN") (GEN tm);;
let GEN_ALL = hilabel_thm_thm (simple_label "GEN_ALL") GEN_ALL;;
let EXISTS t = hilabel_thm_thm (rich_label ("EXISTS", [], [(fst t);(snd t)], [])) (EXISTS t);;
let CHOOSE (v,a) = hilabel_thm_thm (rich_label ("CHOOSE", [], [v], [a])) (fun b -> CHOOSE (v, a) b);;
let SIMPLE_CHOOSE tm = hilabel_thm_thm (rich_label ("SIMPLE_CHOOSE", [],[tm],[])) (SIMPLE_CHOOSE tm);;
let DISJ1 th tm = hilabel_thm_thm (simple_label "DISJ1") (fun th -> DISJ1 th tm) th;;
let DISJ2 tm = hilabel_thm_thm (simple_label "DISJ2") (DISJ2 tm);;
let DISJ_CASES a = 
  hilabel_thm_split' 
    (rich_label ("Case distinction", [], [], [a])) 
    (simple_label "First case") 
    (simple_label "Second case")
    (DISJ_CASES a);;
let SIMPLE_DISJ_CASES = hilabel_thm_split "Case distinction" "First case" "Second case" SIMPLE_DISJ_CASES;;
let NOT_ELIM = hilabel_thm_thm (simple_label "NOT_ELIM") NOT_ELIM;;
let NOT_INTRO = hilabel_thm_thm (simple_label "NOT_INTRO") NOT_INTRO;;
let EQF_INTRO = hilabel_thm_thm (simple_label "EQF_INTRO") EQF_INTRO;;
let EQF_ELIM = hilabel_thm_thm (simple_label "EQF_ELIM") EQF_ELIM;;
let CONTR tm = hilabel_thm_thm (simple_label "CONTR") (CONTR tm);;
let EXISTENCE = hilabel_thm_thm (simple_label "EXISTENCE") EXISTENCE;;
