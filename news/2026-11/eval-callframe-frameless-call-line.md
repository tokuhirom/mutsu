# `callframe` in an EVAL'd unit sees the frames of frameless calls

The frames an `EVAL` inserts (the invoker and the two intermediate frames) are
recorded at routine depth 0, so they were counted as the call frame of the
first routine the EVAL'd unit called. A sub reached through a frameless call
path from the second call on therefore had no frame of its own, and
`callframe(2).line` answered 0 instead of the call site. Those frames no longer
claim a routine frame. `t/vm/frames/callframe-light-call-frames.t` joins the
`EVAL($ast)` ratchet.
