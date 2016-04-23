function createCanvasGraph(canvas, gs, is_collapsed, goal_is_attached) {

    var context;
    var expanded;
    var showgoal;
    var measure_text;
    var PADDING = 10;
    var HEADER_PADDING = 5;
    var HORIZONTAL_GAP = 30;
    var VERTICAL_GAP = 30;
    var LABEL_FONT = '10pt Arial';
    var laidout = {};

    function pushall(vec1, vec2) {
        var i;
        for (i = 0; i < vec2.length; i += 1) {
            vec1.push(vec2[i]);
        }
        return vec1;
    }

    function layout(goalid, render) {
        var box;
        if (!showgoal(goalid)) {
            box = layout_tactic(goalid, render);
            if (render) {
                laidout[goalid] = {x: render.x, y:render.y, width: box.width, height:box.height,
                    goal : null, tactic : {x: render.x, y:render.y, width: box.width, height:box.height}};

            }
            return box;
        }
        var goal = lookupGoal(gs, goalid);
        var goal_dim = render_goal(goal.hyps, goal.goal, false, null);
        var tactic_dim = layout_tactic(goalid, null);
        var width = goal_dim.width;
        if (tactic_dim.width > width) width = tactic_dim.width;
        var height = goal_dim.height + VERTICAL_GAP + tactic_dim.height;
        if (render) {
            laidout[goalid] = {x:render.x, y:render.y, width:width, height:height};

            laidout[goalid].goal = {x:render.x+(width-goal_dim.width)/2, y:render.y,
                width:goal_dim.width, height:goal_dim.height};

            laidout[goalid].tactic = {x:render.x+(width-tactic_dim.width)/2,
                y:render.y+goal_dim.height+VERTICAL_GAP,
                width:tactic_dim.width, height:tactic_dim.height};

            render_goal(goal.hyps, goal.goal, false,
                        {x:render.x+(width-goal_dim.width)/2,
                         y:render.y});

            layout_tactic(goalid,
                          {x:render.x+(width-tactic_dim.width)/2,
                           y:render.y+goal_dim.height+VERTICAL_GAP});
        }
        tactic_dim.width = width;
        tactic_dim.height = height;
        return tactic_dim;
    }

    function render_tactic_header(text, rich_label, expandable, expanded, min_width, additional_height, render) {
        context.font = LABEL_FONT;
        context.lineWidth = 1;
        context.strokeStyle = 'black';
        var ICONSIZE = 14;
        var is_rich = rich_label.terms || rich_label.thms;
        var rheight = 0;
        var m = measure_text(context, text);
        if(expandable) {
            m.width = m.width + ICONSIZE + PADDING;
        }
        var txt_width = m.width;
        if (rich_label.terms) {
            rheight++;
            var mn = measure_text(context, rich_label.terms);
            if (mn.width > m.width) {
                m = mn;
            }
        }
        if (rich_label.thms) {
            rheight++;
            var mn = measure_text(context, rich_label.thms);
            if (mn.width > m.width) {
                m = mn;
            }
        }
        var width = m.width + 2 * PADDING;

        if (width < min_width) {
            width = min_width;
        }

        var height = (1+rheight)*m.height + 2* PADDING + rheight*HEADER_PADDING;
        if (render) {
            rich_label_size = {height: PADDING + rheight*(m.height) + (rheight-1)*+HEADER_PADDING,
                offset: m.height+PADDING+HEADER_PADDING};
            var rich_background;
            if (expandable) {
                context.fillStyle = '#92E6F0';
                rich_background = '#91bef4';
            } else {
                context.fillStyle = '#BAF5C9';
                rich_background = '#7ed2bb';
            }

            context.fillRect(render.x, render.y, width, height + additional_height);
            context.fillStyle = 'black';
            context.textAlign = 'start';
            context.textBaseline = 'top';
            var x = render.x + PADDING;
            var y = render.y + PADDING;

            if (is_rich) {
                x = render.x + (width - txt_width)/2;
            }

            if (expandable) x += ICONSIZE+PADDING;

            context.fillText(text,x,y);
            if(is_rich) {
                console.log(rich_label,render.rich_label);
                context.fillStyle = rich_background;
                context.fillRect(render.x, render.y+rich_label_size.offset, width, rich_label_size.height);
                context.fillStyle = '#000000';
                rich_y = render.y+rich_label_size.offset + HEADER_PADDING;
                if(rich_label.terms) {
                    context.fillText(rich_label.terms, render.x+PADDING, rich_y);
                    rich_y += m.height + HEADER_PADDING;
                }
                if (rich_label.thms) {
                    context.fillText(rich_label.thms, render.x+PADDING, rich_y);
                }
            }
            context.strokeRect(render.x, render.y, width, height + additional_height);
            if (expandable) {
                context.fillStyle = 'white';
                context.fillRect(render.x+PADDING, render.y+PADDING, ICONSIZE, ICONSIZE);
                context.strokeRect(render.x+PADDING, render.y+PADDING, ICONSIZE, ICONSIZE);
                var D = 3;
                if (expanded) {
                    context.strokeRect(render.x+PADDING+D, render.y+PADDING+D+(ICONSIZE-2*D)/2, ICONSIZE-2*D, 1);
                } else {
                    context.strokeRect(render.x+PADDING+D, render.y+PADDING+D+(ICONSIZE-2*D)/2-0.5, ICONSIZE-2*D, 1);
                    context.strokeRect(render.x+PADDING+D+(ICONSIZE-2*D)/2-0.5, render.y+PADDING+D, 1, ICONSIZE-2*D);
                }
            }
        }
        return {width : width, height : height};

    }

    /* Returns an object { width, height, outgoing }.
     Params:
     goalid - the id of the goal to render
     render - either null or has x and y component
     */
    function layout_tactic(goalid, render) {
        var goal = lookupGoal(gs, goalid);
        context.font = LABEL_FONT;
        context.lineWidth = 1;
        var outgoing = pushall([], goal.outgoing);
        //console.log('goalid = '+goalid+ ', outgoing='+outgoing.length+', tac='+goal.tactic);
        var expandable = goal.children.length > 0;
        if (expanded(goalid) && expandable) {
            var result = layout_children(goalid, null);
            var m = render_tactic_header(goal.tactic, goal.rich_label, true, true, result.width + 2 * PADDING, result.height + 2 * PADDING, render);
            var width = m.width;
            var height = m.height;
            var total_height = height + PADDING + result.height + PADDING;
            if (render) {
                context.fillStyle = '#DCF2F5';
                context.fillRect(render.x, render.y+height, width, total_height-height);
                context.strokeStyle = 'black';
                context.strokeRect(render.x, render.y, width, total_height);
                layout_children(goalid, { x : render.x + width/2-result.width/2,
                                y : render.y + height + PADDING});
            }
            return  {
                        width : width,
                        height : total_height,
                        outgoing : pushall(outgoing, result.outgoing)
                    };
        } else {
            var text = goal.tactic;
            if (expandable) {
                pushall(outgoing, layout_children(goalid, null).outgoing);
            }
            var m = render_tactic_header(goal.tactic, goal.rich_label, expandable, false, 0, 0, render);
            return {
                    width : m.width,
                    height : m.height,
                    outgoing : outgoing
                   };
        }
    }

    function layout_from(goalid, render) {
        //return layout(goalid, render);
        var result = layout(goalid, null);
        var parentid = lookupGoal(gs, goalid).parent_id;
        var outgoing = [];
        var row = [];
        var outs = result.outgoing;
        var i;
        for (i = 0; i < outs.length; i += 1) {
            var id = outs[i];
            if (isAncestor(gs, parentid, id)) {
                row.push(id);
            } else {
                outgoing.push(id);
            }
        }
        if (row.length == 0) {
            if (render) {
                return layout(goalid, render);
            } else {
                return result;
            }
        } else {
            var row_result = layout_row(row, null);
            var width = result.width;
            if (row_result.width > width) width = row_result.width;
            var height = result.height + VERTICAL_GAP + row_result.height;
            pushall(outgoing, row_result.outgoing);
            if (render) {
                var r = {};
                r.x = render.x + (width - result.width) / 2;
                r.y = render.y;
                layout(goalid, r);
                r.x = render.x + (width - row_result.width) / 2;
                r.y = render.y + result.height + VERTICAL_GAP;
                layout_row(row, r);
            }
            return {width : width, height : height, outgoing : outgoing};
        }
    }

    function layout_row(ids, render) {
        var x = 0, height = 0;
        var outgoing = [];
        var i;
        for (i = 0; i < ids.length; i += 1) {
            if (i > 0) x += HORIZONTAL_GAP;
            var result;
            if (render) {
                result = layout_from(ids[i], {x : render.x + x, y : render.y});
            } else {
                result = layout_from(ids[i], null);
            }
            x += result.width;
            if (result.height > height) height = result.height;
            pushall(outgoing, result.outgoing);

        }
        return {width: x, height: height, outgoing : outgoing};
    }

    function layout_children(parent_id, render) {
        //console.log('layout the children of '+parent_id);
        var children = computeChildren(gs, parent_id);
        var top_child = [];
        for (i = 0; i < children.length; i += 1) {
            if (!isAncestor(gs, parent_id, children[i].incoming)) {
                top_child.push(children[i].id);
            }
        }
        //console.log('number of top children: '+top_child.length);
        return layout_row(top_child, render);
    }

    function getcoors(dim, s) {
        return {x:dim.x+dim.width/2,y:dim.y+s*dim.height};
    }

    var arrows = [];

    function draw_arrow(from_id, to_id) {
        context.lineWidth = 2;
        //console.log('draw_arrow from '+from_id+' to '+to_id);
        var src = getcoors(laidout[from_id],1);
        var dest = getcoors(laidout[to_id],0);
        context.strokeStyle = 'black';
        context.beginPath();
        context.moveTo(src.x, src.y+1);
        context.lineTo(dest.x, dest.y-1);
        context.stroke();
        arrows.push({x1:src.x, y1:src.y+1, x2:dest.x, y2:dest.y-1,
                   from_id:from_id, to_id:to_id});
    }

    function simply_draw_arrow(from_dim, to_dim) {
        var src = getcoors(from_dim,1);
        var dest = getcoors(to_dim,0);
        context.lineWidth = 2;
        context.strokeStyle = 'black';
        context.beginPath();
        context.moveTo(src.x, src.y+1);
        context.lineTo(dest.x, dest.y-1);
        context.stroke();
    }

    function search_arrow(x, y) {
        function dist(x1, y1, x2, y2) {
            var dx = x2-x1, dy = y2-y1;
            return Math.sqrt(dx*dx + dy*dy);
        }
        var i;
        for (i = 0; i < arrows.length; i += 1) {
            var arrow = arrows[i];
            var l = dist(arrow.x1, arrow.y1, arrow.x2, arrow.y2);
            var d1 = dist(x, y, arrow.x1, arrow.y1);
            var d2 = dist(x, y, arrow.x2, arrow.y2);
            var d = Math.abs((x - arrow.x1)*(arrow.y2 - arrow.y1) -
                             (y - arrow.y1)*(arrow.x2 - arrow.x1))/l;
            if (d1 < l && d2 < l && d < 2)
                return arrow;
        }
        return null;
    }

    function laidoutAncestor(id) {
        if (laidout[id]) {
            return id;
        } else {
            return laidoutAncestor(lookupGoal(gs, id).parent_id);
        }

    }

    function draw_arrows() {
        arrows = [];
        var i;
        for (i=0; i<gs.goalids.length; i += 1) {
            var id = gs.goalids[i];
            if (laidout[id]) {
                var goal = lookupGoal(gs, id);
                if (goal.incoming) {
                    draw_arrow(laidoutAncestor(goal.incoming), id);
                }
                if (laidout[id].goal) {
                    simply_draw_arrow(laidout[id].goal, laidout[id].tactic);
                }
            }
        }
    }


    function find_goalid_at(x, y, lookup) {
        var r = null;
        var found = null;
        var i;
        for (i=0; i<gs.goalids.length; i += 1) {
            var id = gs.goalids[i];
            var q = lookup(id);
            if (q && x >= q.x && x <= q.x+q.width && y >= q.y && y <= q.y + q.height) {
                if (r) {
                    if (lookupGoal(gs, id).level > lookupGoal(gs, found).level) {
                        found = id;
                        r = q;
                    }
                } else {
                    r = q;
                    found = id;
                }
            }
        }
        return found;
    }

    function render_multiline_text(context, h, render) {
        context.textAlign = 'start';
        context.textBaseline = 'top';
        var hs = h.split('\n');
        var i;
        var dim = {width: 0, height: 0};
        for (i = 0; i<hs.length; i++) {
            if (render) context.fillText(hs[i], render.x, render.y + dim.height);
            d = measure_text(context, hs[i]);
            dim.height += d.height;
            if (d.width > dim.width) dim.width = d.width;
        }
        return dim;
    }

    function render_goal(hyps, goal, border, pos) {
        context.font = LABEL_FONT;
        context.lineWidth = 1;
        context.strokeStyle = 'black';
        var i;
        var dim;
        var max_left = 0, max_right = 0;
        var height = 0;
        context.textBaseline = 'top';
        for (i = 0; i < hyps.length; i += 1) {
            var h = hyps[i];
            dim = render_multiline_text(context, h, null);
            if (dim.width > max_right) max_right = dim.width;
            height = height + dim.height;
            dim = measure_text(context, '' + (i+1)+')');
            if (dim.width > max_left) max_left = dim.width;
        }
        var width = max_left + PADDING + max_right;
        dim = render_multiline_text(context, goal, null);
        if (dim.width > width) width = dim.width;
        width += 2 * PADDING;
        var box = {};
        box.width = width;
        box.height = height + dim.height + 2*PADDING;
        if (hyps.length > 0) box.height += PADDING;
        if (pos) {
            context.fillStyle = 'white';
            if (border) {
                context.fillRect(pos.x, pos.y, box.width, box.height);
                context.strokeRect(pos.x, pos.y, box.width, box.height);
            }
            context.fillStyle = 'black';
            var y = pos.y + PADDING;
            for (i = 0; i < hyps.length; i += 1) {
                dim = render_multiline_text(context, hyps[i],
                                            {x: pos.x+PADDING+max_left+PADDING, y: y});
                context.textAlign = 'end';
                context.fillText(''+(i+1)+')', pos.x+PADDING+max_left, y);
                y += dim.height;
            }
            if (hyps.length > 0) {
                context.strokeRect(pos.x+PADDING, y+PADDING/2, width - 2*PADDING, 1);
                y += PADDING;
            }
            render_multiline_text(context, goal, {x: pos.x+PADDING, y: y});
        }
        return box;
    }

    function rendergraph(canvas) {
        laidout = {};
        var result = layout_children(null, null);
        var w = Math.ceil(result.width);
        var h = Math.ceil(result.height);
        canvas.width = w;
        canvas.height = h;
        layout_children(null, {x:0, y:0});
        draw_arrows();
    }

    function render(canvas)
    {
        context = canvas.getContext('2d');

        expanded = function(id) {
            if (is_collapsed[id])
                return true;
            else return false;
        };

        showgoal = function(id) {
            if (goal_is_attached[id] || is_root_goal(gs, id))
                return true;
            else return false;
        };


        measure_text = function(ctx, label) {
            var w = ctx.measureText(label).width;
            var h = ctx.measureText('M').width * 1.2;
            return { width: w, height : h};
        };

        rendergraph(canvas);

        var hovering_goal = null;

        canvas.onmousedown = function(e) {
            if (hovering_goal) {
                goal_is_attached[hovering_goal] = true;
                hovering_goal = null;
                rendergraph(canvas);
                return;
            }
            var loc = CanvasTools.windowToCanvas(canvas, e.clientX, e.clientY);
            var id = find_goalid_at(loc.x, loc.y, function(id) {
                                if (laidout[id]) return laidout[id].goal; else return null;
                                });
            if (id) {
                goal_is_attached[id] = false;
                rendergraph(canvas);
                return;
            }
            id = find_goalid_at(loc.x, loc.y, function(id) {
                                    if (laidout[id]) return laidout[id].tactic; else return null;
                                    });
            if (id) {
                if (is_collapsed[id]) is_collapsed[id] = false;
                else is_collapsed[id] = true;
                rendergraph(canvas);
                return;
            }
        };

        function findArrow(e) {
            var loc = CanvasTools.windowToCanvas(canvas, e.clientX, e.clientY);
            var on_arrow = search_arrow(loc.x, loc.y);
            if (on_arrow) return on_arrow.to_id;
            //var id = find_goalid_at(loc.x, loc.y, function(id) {
                                    //if (laidout[id]) return laidout[id].tactic; else return null;
                                    //});
            //return id;
            return null;
        }

        function startedHovering(f, e) {
            if (showgoal(f)) return;
            if (hovering_goal) return;
            hovering_goal = f;
            console.log('started hovering: '+f);
            var g = lookupGoal(gs, f);
            var loc = CanvasTools.windowToCanvas(canvas, e.clientX, e.clientY);
            var dim = render_goal(g.hyps, g.goal, true, null);
            render_goal(g.hyps, g.goal, true, {x : loc.x - dim.width/2, y: loc.y - dim.height/2});
        }

        function stoppedHovering(f, event) {
            console.log('stopped hovering: '+f);
            hovering_goal = null;
            rendergraph(canvas);
        }


        canvas.onmousemove =
        CanvasTools.hoverEventHandler(500, 100, 0,
                                      findArrow,
                                      startedHovering,
                                      stoppedHovering);
    };

    render(canvas);

    return laidout;
}
