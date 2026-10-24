# `*` and `\()` parameter defaults are constants

A parameter default of `*` or an empty capture literal `\()` is now bound
without evaluating it, and keeps the call on the light call path:
`sub f($a, $p = *) { ... }` costs about 4.1k instructions per call instead of
about 29.5k (callgrind, warm). `CompiledFunction::constant_default_value` is
the one place that says which defaults are constants; the light call path's
fill table, `eval_param_default` and the precompiled-chunk planner all use it.
`()`, `[]`, `{}` and enum-constant defaults still go through the evaluation
(`()` because `infix:<,>` can be overloaded, the containers because each call
needs a fresh one). #14854
