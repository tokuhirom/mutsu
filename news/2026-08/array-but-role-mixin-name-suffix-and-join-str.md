# A role-mixed value's `Str` now survives every string-coercion path, and an anonymous mixin names itself

Two symptoms were tracked here: `(@a but role { method Str {...} }).^name` reported a bare
`Array` instead of raku's `Array+{<anon|1>}`, and `join(">", $r)` rendered the base list
(`3 2 1`) while `print $r` on the very same value correctly rendered the role's `Str`
(`3<2<1`). Both are fixed, and the second turned out to be the shared root cause of this whole
family of tickets.

## Root cause 1 (the family's shared cause): coercion sites tested `ValueView::Instance` and
silently downgraded `ValueView::Mixin`

The discriminating pair in the ticket — `print` works, `join` does not — is exactly the split
between the two ways mutsu turns a value into a string. `print`/`put`/`say` go through
`Interpreter::render_str_value` / `render_gist_value`, which dispatch `.Str`/`.gist` as real
methods and therefore see a composed role. Every *other* string-coercion site was written as
"if this is a `ValueView::Instance`, dispatch its `Stringy`/`Str`; otherwise use the pure
`to_str_context()`", and a role-mixed value is a `ValueView::Mixin`, not an `Instance` — so it
fell through to the base rendering. The same downgrade was present at five independent sites:

- `exec_str_coerce_op` (`src/vm/vm_misc_coerce.rs`) — prefix `~`
- `coerce_stringy_operand` (`src/vm/vm_coerce_concat_ops.rs`) — infix `~` and `eq`/`lt`/…
- `exec_string_concat_op` (`src/vm/vm_var_assign_typed.rs`) — `"$x"` interpolation
- `builtin_sprintf` (`src/runtime/builtins_string.rs`) plus `native_sprintf`'s pure fast path
  (`src/builtins/functions/sprintf_fmt.rs`), which bailed out for `Instance` and `Package` args
  but not for `Mixin`
- `join` — here with an extra twist: the pure two-argument fast path
  (`src/builtins/functions/dispatch_2arg.rs`) answered before the interpreter's `builtin_join`
  was ever consulted, so `join` was broken for a plain class instance with a user `method Str`
  too (`join(",", $c)` rendered `C()`), not only for a mixin.

The fix introduces one shared oracle, `Interpreter::mixin_user_stringifier`
(`src/runtime/methods_mixin_dispatch.rs`): it returns the composition's own `Stringy`/`Str`
(including a `but`-mixed *value* override), else the wrapped value's own class-declared one,
else `None` — and `None` means "keep the native rendering", so an `Array but SomeMarkerRole`
still stringifies as a plain list. Each coercion site consults it before its `Instance` arm.
`join` additionally gained `join_needs_interpreter` (`src/builtins/functions/flat.rs`), which
makes the pure fast path decline when any element may carry a user stringifier, and
`Interpreter::join_prerender_user_stringifier`, which dispatches those elements before the pure
`join_flat` walk sees them.

## Root cause 2: anonymous roles were deliberately masked out of the `+{...}` name

`role_mixin_suffix_excluding` (`src/value/types.rs`) filtered out every role whose registry name
started with `__ANON_ROLE_`, on the reasoning that mutsu's generated id would not match Rakudo's.
But dropping the entry entirely reports a bare `Array`, which loses every trace of the
composition; Rakudo reports `Array+{<anon|1>}`. `crate::value::user_facing_type_name` already
knew how to render `__ANON_ROLE_{id}__` as `<anon|N>` (it does exactly that for an anonymous
`class`/`grammar` in a gist), so the suffix builder now routes through it. In practice mutsu's
ids line up with Rakudo's on these repros anyway; the pinned tests assert the shape rather than
the id.

The same helper also learned to append a parameterised role's type arguments, so
`5 but G[Int]` reports `Int+{G[Int]}` (was `Int+{G}`) and `%h but Associative[Int,Int]` reports
`Hash+{Associative[Int,Int]}`.

## Verification

`t/role-mixin-survival.t` (53 assertions) pins the whole matrix — naming, `~`/`~`-infix/
interpolation/`join`/`sprintf`/`eq`, gist, `:=` binding, `.sort`/`.map`/`.grep`, built-in
parametric roles, and value-mixin-vs-allomorph — and passes identically under `raku` and
`mutsu`. `t/decl-mixin-begin.t` and `t/metamodel-set-name.t` each had one assertion pinning the
old anon-masking behaviour; both were re-measured against `raku` and corrected.
