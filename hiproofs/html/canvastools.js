CanvasTools = {

    windowToCanvas : 
    function (canvas, x, y) {
        var bbox = canvas.getBoundingClientRect();
        
        return {
        x: x - bbox.left * (canvas.width / bbox.width) ,
        y: y - bbox.top * (canvas.height / bbox.height)
        };
    },
    
    hoverEventHandler : 

    
    function hoverEventHandler(time_before, time_recheck, time_after,
                               find, startedHovering, stoppedHovering)
    {
        var found;
        var found_index = 0;
        hovered_over = {};
        function handle(e) {
            var next_found = find(e);
            if (found != next_found) {
                found_index += 1;
                found = next_found;
            }
            function done(f, event) {
                stoppedHovering(f, event);
            }
            function timeout(f, findex, hovering, event) {
                if (!hovering && hovered_over[f]) return;
                if (findex == found_index) {
                    if (!hovering) {
                        startedHovering(f, event);
                    }
                    hovered_over[f] = true;
                    window.setTimeout(timeout, time_recheck, f, findex, true, event);
                } else {
                    hovered_over[f] = false;
                    if (hovering) {
                        window.setTimeout(done, time_after, f, event);
                    }
                }
                
            }
            if (found) {
                window.setTimeout(timeout, time_before, found, found_index,false, e);
            }
        };
        return handle;
    }

};