HipCam
======

Tools for recording Hiproofs in hol-light and visualising them in a web browser.

More extensive documentation on how to use these tools is coming soon.
There is also a forthcoming paper about it. For some high-level information available now, see the slides in the file *hiproofs_talk.pdf*. 

If you are eager to use it NOW, do the following:

* Make sure you have OCaml 4.00 or better (we've used GADTs)
* Install HOL Light (see hol.ml to see if the versions match)
* Copy the hiproofs directory and fusion.ml and hol.ml into the HOL Light directory

Make sure that the variable JGRAPH_BROWSER_COMMAND in hiproofs/main.ml
is set properly for your environment. The default value is

    open $
    
which uses the Mac OS X's `open` command to show the generated HTML in
the default web browser. The dollar sign $ is where the location of
the HTML file to display will be inserted.


You can now export a theorem *T* via

    Hitools.export *size* *T* *name*
    
For adding your own hierarchical boxes to proofs, you can use the
**hilabel** function or any of its derivatives:

    hilabel : Hiproofs.label -> (thm list -> thm) -> thm list -> thm
    hilabel_thm : Hiproofs.label -> thm -> thm
    hilabel_tac : Hiproofs.label -> tactic -> tactic
    hilabel_tac_goals : Hiproofs.label list -> tactic -> tactic
    hilabel_tac_frame : Hiproofs.label -> tactic -> tactic
    
    string_label : string -> Hiproofs.label

Have fun visualising!
