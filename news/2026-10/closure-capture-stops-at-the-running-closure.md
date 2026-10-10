# A closure made inside a closure no longer captures its callers

A closure records the names it can see when it is created. When it was
created while another closure's body was running, that record also took in
every frame that had called that body. Those callers' names are lexically
invisible to the new closure. Taking them made the capture grow with the
dynamic nesting of closure calls, and it had to be compacted once it passed
eight layers.

Call frames now mark their root tier. A closure created inside a closure body
stops collecting at that body's frame: it takes the body's own scope and the
body's own capture, plus the program scope at the bottom of the chain. The
capture of a closure created at the bottom of 40 nested closure calls is now as
small as one created at depth 5. This is the first step of ADR-12529 phase 3
(#12519).
