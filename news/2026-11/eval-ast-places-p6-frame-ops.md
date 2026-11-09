# `EVAL($ast)` places `nqp::p6stateinit` / `nqp::p6takefirstflag`

The frame-reading `nqp::p6*` ops are lowered to hidden frame variables by a pass over
the whole unit (`parser::p6_frame_state`). A source parse and the `.AST` round trip ran
it, but a tree lowered by `EVAL($ast)` did not, so `nqp::p6stateinit()` died with
`Unsupported nqp:: op`. The lowering now runs the same pass;
`t/vm/nqp-p6-frame-state-ops.t` joins the `EVAL($ast)` ratchet.
