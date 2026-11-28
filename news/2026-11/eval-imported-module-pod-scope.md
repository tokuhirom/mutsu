An imported module routine called during EVAL now reads the module's own
`$=pod`. The EVAL unit and its routines continue to read their own document.
This also lets `t/modules/module-pod-late-call.t` pass through AST EVAL.
