# Routines with scalar rw params register body-less (C6e-3c prep)

The C6e-3b safe-class empty-body registration excluded routines with a
scalar `is rw`/`is raw` parameter: their calls were gated onto the
interpreter carrier (which executes the AST body), so registering them
body-less would have broken every such call. With shared-cell rw binding
(`news/2026-08/rw-params-bind-shared-cells.md`) those routines run their
compiled bodies on every path, so the keep-class is lifted:
`vm_register_sub_ops.rs`'s registration predicate no longer checks
`has_rw_scalar_param`, and a plan-derived rw-param routine registers with
an empty body like any other safe-class def.

The remaining C6e-3c keep-classes are down to: a plan without resolvable
bytecode for every declared signature (class-walker nested subs),
routine-level `is rw`/`is raw`/tail-`return-rw` lvalue routines (the
assignment machinery extracts the target from the AST), and NativeCall
marshalling traits — tracked in
`todo/deep/c6e-legacy-body-drop-blocked-by-gate-rejected-shapes.md`.

Validated with the full local chain (t/ suite, `make roast`, battery
gate); the rw behavior itself is pinned by `t/rw-shared-cell.t` and
`t/proto-dispatch-interpreter-path.t`.
