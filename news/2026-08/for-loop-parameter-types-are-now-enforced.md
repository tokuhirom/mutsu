# A `for` loop now enforces its declared parameter types

```raku
for ("a", 1, "b", "two") -> Str $k, Int $v { say "$k=$v" }
# raku:  a=1
#        Type check failed in binding to parameter '$v'; expected Int but got Str ("two")
```

Was silently accepted before, both for this multi-param shape (originally
diagnosed in `todo/tickets/for-loop-multi-param-types-unenforced.md`) and,
it turned out while implementing the fix, for the single-param form too
(`for @a -> Int $x`) — neither went through any enforcement path at all.

## Root cause and where it actually needed fixing

The originally-proposed fix (reuse `bind_param_type_constraint` plus the
existing `SetLocal` assignment type-check that the multi-param bind-prefix
already runs through) would have raised the wrong exception class:
`SetLocal`'s check throws `X::TypeCheck::Assignment` (verified against raku
for `my Int $v; $v = "two"`), but a `for`-loop parameter binding failure is
`X::TypeCheck::Binding::Parameter` in raku — a real semantic difference for
`CATCH { when X::TypeCheck::Binding::Parameter { ... } }`-style code, not
cosmetic.

Fixed instead with an explicit per-iteration check in Rust, at the one place
in `src/vm/vm_for_loop_body.rs` (`exec_for_loop_body`) where the
per-iteration item is available before either bind path runs: the
single-param case is already bound directly in Rust there
(`self.env_mut().insert(name, item)`, not via compiled bytecode); the
multi-param case reads a `ValueView::Array` chunk (`Value::array(chunk.to_vec())`,
built by `arity > 1`'s `.chunks()`) whose N-th element is exactly what the
compiled bind-prefix `Stmt::Assign` later reads via `$_[i]`.

Added `ForLoopSpec::param_type_constraint: Option<String>` (single) and
`multi_param_type_constraints: Vec<Option<String>>` (parallel to
`multi_param_names`), populated from `param_def`/`params_def` at compile
time. New `RuntimeError::typecheck_binding_parameter_with_repr` builds the
exact raku wording (`value_short_repr`/`got_type_name`, matching
`type_check_binding_typed_error`'s existing pattern) with NO class-name
prefix baked into the message — unlike several older sibling constructors in
the same file, whose baked-in prefix means `.message`/`.Str` don't quite
match raku's own text. `Self::typed` copies the message string verbatim into
both the top-level uncaught display and `.message`/`.Str`.

Verified still working: coercion types (`Str(Cool) $v`), sigilless `\v`
params, `is rw`/`<->` params (single- and multi-param), and a short chunk
with a declared default (`-> Str $k, Int $v = 99` called with one item —
exempt from the check, same as an unpassed routine optional). The existing
`t/for-multi-param-type-constraint.t` (untyped-param shadowing semantics,
unrelated to this fix) still passes unchanged.

Regression test: `t/for-loop-param-type-enforced.t`.
