function throwGraphStructError(location, message) {
    throw {
        name : "GraphStruct Error",
        location : location,
        message : message
    };
};

function createGraphStruct() {
    return {
        goals : {},
        goalids : []
    };
};

function lookupGoal(graphstruct, id) {
    if (id) {
        return graphstruct.goals[id];
    } else {
        throwGraphStructError("lookupGoal", "no such id");
    }
};

function defineNode(graphstruct, parent_id, id, hyps, goal, tactic, rich_label) {
    if (lookupGoal(graphstruct, id))
        throwGraphStructError("defineGoal", "a goal with this id has already been defined");
    var parent = null;
    if (parent_id) {
        parent = lookupGoal(graphstruct, parent_id);
        if (parent) {
            parent.children.push(id);
        } else
            throwGraphStructError("defineGoal", "parent goal not found");
    }
    var level = 0;
    var p = parent_id;
    while (p) {
        var p_goal = lookupGoal(graphstruct, p);
        level = level + 1;
        p = p_goal.parent_id;
    }
    if (rich_label == undefined) {
        rich_label = {};
    }
    var g = {
        id : id,
        level : level,
        hyps : hyps,
        goal : goal,
        tactic : tactic,
        children : [],
        parent_id : parent_id,
        incoming : null,
        outgoing : [],
        rich_label: rich_label
    };
    graphstruct.goals[id] = g;
    graphstruct.goalids.push(id);
    return g;
};

function computeCommonParent(gs, from_goal_id, to_goal_id) {
    var from_goal = lookupGoal(gs, from_goal_id);
    var to_goal = lookupGoal(gs, to_goal_id);
    while (from_goal.level > to_goal.level) {
        from_goal = lookupGoal(gs, from_goal.parent_id);
    }
    while (to_goal.level > from_goal.level) {
        to_goal = lookupGoal(gs, to_goal.parent_id);
    }
    while (from_goal.level > 0 && from_goal.parent_id != to_goal.parent_id) {
        from_goal = lookupGoal(gs, from_goal.parent_id);
        to_goal = lookupGoal(gs, to_goal.parent_id);
    }
    return { from_goal: from_goal, to_goal: to_goal, parent_id: from_goal.parent_id };
};

function isAncestor(gs, ancestor_id, id) {
    if (ancestor_id == null) return id != null;
    while (id) {
        if (id == ancestor_id) return true;
        id = lookupGoal(gs, id).parent_id;
    }
    return false;
}

function addEdge(graphstruct, from_goal_id, to_goal_id) {
    var from_goal = lookupGoal(graphstruct, from_goal_id);
    var to_goal = lookupGoal(graphstruct, to_goal_id);
    if (!from_goal) throwGraphStructError("defineEdge", "missing from_goal");
    if (!to_goal) throwGraphStructError("defineEdge", "missing to_goal");
    if (to_goal.incoming)
        throwGraphStructError("defineEdge", "to_goal has already an incoming edge");
    if (from_goal.outgoing.indexOf(to_goal_id) != -1)
        throwGraphStructError("defineEdge", "from_goal already has this outgoing edge");
    to_goal.incoming = from_goal_id;
    from_goal.outgoing.push(to_goal_id);
};

function defineEdge(gs, from_goal_id, to_goal_id) {
    var common = computeCommonParent(gs, from_goal_id, to_goal_id);
    var to_id = common.to_goal.id;
    addEdge(gs, from_goal_id, to_id);
};

function is_root_goal(gs, goal_id) {
    var goal = lookupGoal(gs, goal_id);
    return goal && !goal.parent_id && !goal.incoming;
}

function computeChildren(gs, parent_id) {
    var children;
    var i;
    if (parent_id == null) {
        children = [];
        for (i = 0; i < gs.goalids.length; i += 1) {
            var id = gs.goalids[i];
            var goal = lookupGoal(gs, id);
            if (goal.parent_id == null) {
                children.push(goal);
            }
        }
    } else {
        var children_ids = lookupGoal(gs, parent_id).children;
        children = [];
        for (i = 0; i < children_ids.length; i += 1) {
            children.push(lookupGoal(gs, children_ids[i]));
        }
    }
    return children;
};

