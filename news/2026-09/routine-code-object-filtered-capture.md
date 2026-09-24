# `&f` no longer costs O(frame size): a routine's code object gets a filtered capture

Reading a registered routine as a value (`&f`, and every other path through
`sub_value_from_function_def`) built its `Sub` around `self.env.clone()`: the
whole live env. The clone itself is only an `Arc` bump, but it left the running
frame's leaf tier shared, so the frame's next by-name write copied that whole
tier. Every `&f` therefore cost O(v), v = env entries of the current scope. In
`scripts/vm-complexity-check.sh '&f read'`, 20000 `$c = &f` reads took 0.47 s
next to 500 locals and 0.87 s next to 1000 (ratio 1.84). Rakudo does this in
O(1).

A compiled routine only reaches that env through `call_compiled_closure`,
which installs the env's leaf tier as the call's capture fallback. That is the
same role a closure's captured env plays. So the code object now gets the same
filtered capture a closure gets (`Interpreter::routine_code_object_env`): the
routine's free variables, their `__mutsu_type::` shadows, and every system
name. The key filter is now one function, `vm_register_ops::capture_keeps`,
shared by both captures. Two cases keep the whole env as before:

- a body that can name a lexical dynamically (`EVAL`, `CALLER::`, symbolic
  deref), using the same `needs_reflective_capture` gate the closure capture
  uses;
- a def with no compiled body, because its AST carrier merges the entire
  captured env into the call.

After the change the same case measures 0.064 s / 0.066 s (ratio 1.03). The
other `#9169` cases stay flat.

The same PR brings the `ButMixin`/`Does`/`DoesVar`/`SmartMatchExpr`
dispatch-arm `// Cost:` lines back in line with their handlers, which #9276 had
already made independent of the frame size. The only `#9169` suffix left is the
residual whole-frame publish of `~~` for a code-bearing regex, `s///`/`tr///`
or a junction/collection RHS.

Pin: `t/routines/call/routine-code-object-capture.t`. It covers mainline and
routine free variables, dynamics, lexical subs called by name, a thread, state
shared between `&f` and by-name calls, recursion, and late writes to a captured
lexical.
