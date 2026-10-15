# A method call with an argument is within 1.2x of a zero-argument call

`$o.m($x)` against `$o.m()` (callgrind, profiling build, 5001 vs 1 iterations, empty-loop skeleton of
3,381 Ir subtracted, warm second runs): 1.223x (16,605 vs 13,581 Ir/call) before, **1.194x** (16,271 vs
13,630 Ir/call) after, which meets the #10959 close condition (at most 1.2x).

Two removals on the fast method path (`call_compiled_method_fast_in_unit`):

- the bound `(name, symbol, value)` parameters were collected into a `Vec`, which allocated on the
  first push, so every call with an argument paid a malloc/free pair; one or two parameters now live
  inline (`vm_param_values.rs`) and only a longer list spills (-206 Ir/call);
- `reconcile_captured_readonly_ex` walked three empty free-variable lists for a method that writes no
  free variable whenever any readonly name (here the callee's own `$x`) was live; it now returns first
  (-130 Ir/call).
