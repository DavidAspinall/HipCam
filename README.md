hiproofs
========

Tools for recording Hiproofs in hol-light and visualising them in a web browser.

More extensive documentation on how to use these tools is comming soon.
There is also a forthcoming paper about it. 

If you are eager to use it NOW, do the following:

* Install HOL Light (see hol.ml to see if the versions match).
* Copy the hiproofs directory into the HOL Light directory
* Copy the files in hollight-modifications into the HOL Light directory, overwriting the original ones.    
  Only fusion.ml, tactics.ml and hol.ml are necessary modifications, but the more modifications, the better the quality of the recorded hiproofs.      
  
Make sure that the variable JGRAPH_BROWSER_COMMAND is set properly for your environment. The default value is

    open "/Applications/Google Chrome.app" $
    
which uses Chrome on Mac OS X as a browser. The dollar sign $ is where the location of the HTML file to display will be inserted.


You can now export a theorem T via

    Hitools.export T name
    
For adding your own labelled boxes to proofs, you can use the **hilabel** function or any of its derivatives:

    hilabel_thm
    hilabel_thm_str
    hilabel_tac
    hilabel_tac_str
    hilabel_tac_goals
    hilabel_tac_frame

Have fun visualising!
