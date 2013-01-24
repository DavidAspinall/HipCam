function defgraph() {
    gs = createGraphStruct();
    defineNode (gs, null, 12, [], "!x y. x * y = &1 ==> inv y = x", "REPEAT GEN_TAC");
    defineNode (gs, 12, 1, [], "!x y. x * y = &1 ==> inv y = x", "GEN_TAC");
    defineNode (gs, 12, 2, [], "!y. x * y = &1 ==> inv y = x", "GEN_TAC");
    defineEdge (gs, 1, 2);
    defineNode (gs, null, 3, [], "x * y = &1 ==> inv y = x", "ASM_CASES_TAC");
    defineNode (gs, null, 4, ["y = &0"], "x * y = &1 ==> inv y = x", "ASM_REWRITE_TAC");
    defineNode (gs, null, 5, ["~(y = &0)"], "x * y = &1 ==> inv y = x", "ASM_REWRITE_TAC");
    defineNode (gs, null, 11, ["~(y = &0)"], "x * y = &1 ==> inv y = x", "FIRST_ASSUM (SUBST1_TAC o SYM o (MATCH_MP REAL_MUL_LINV))");
    defineNode (gs, 11, 6, ["~(y = &0)"], "x * y = &1 ==> inv y = x", "SUBST1_TAC");
    defineNode (gs, null, 7, ["~(y = &0)"], "x * y = inv y * y ==> inv y = x", "ASM_REWRITE_TAC");
    defineNode (gs, null, 10, ["~(y = &0)"], "x = inv y ==> inv y = x", "DISCH_THEN (ACCEPT_TAC o SYM)");
    defineNode (gs, 10, 8, ["~(y = &0)"], "x = inv y ==> inv y = x", "DISCH_TAC");
    defineNode (gs, 10, 9, ["x = inv y", "~(y = &0)"], "inv y = x", "ACCEPT_TAC");
    defineEdge (gs, 8, 9);
    defineEdge (gs, 7, 8);
    defineEdge (gs, 6, 7);
    defineEdge (gs, 5, 6);
    defineEdge (gs, 3, 4);
    defineEdge (gs, 3, 5);
    defineEdge (gs, 2, 3);
    return {name: "example1", gs: gs};
}

