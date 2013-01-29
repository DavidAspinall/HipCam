(* meson.ml *)
let ASM_MESON_TAC ths = hilabel_tac false (rich_label ("ASM_MESON_TAC", [], [], ths)) (ASM_MESON_TAC ths);;
let MESON_TAC ths = hilabel_tac false (rich_label ("MESON_TAC", [], [], ths)) (MESON_TAC ths);;
