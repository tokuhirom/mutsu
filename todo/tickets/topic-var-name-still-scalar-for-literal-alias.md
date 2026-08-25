# `$_.VAR.^name` is `Scalar` for a topic aliased to a literal (raku: the value's type)

Found while closing `bind-scalar-literal-var-name-not-int.md`
(`news/2026-08/bind-to-literal-has-no-container.md`).

## Repro

```
$ raku -e 'for 1,2 { say $_.VAR.^name; last }; given 5 { say $_.VAR.^name }'
Int
Int
$ target/debug/mutsu -e 'for 1,2 { say $_.VAR.^name; last }; given 5 { say $_.VAR.^name }'
Scalar
Scalar
```

A topic aliased to a *container* is correctly `Scalar` in both
(`my @a = 1,2; for @a { $_.VAR.^name }`, `my %h = a=>1; for %h.values { ... }`).

## Root cause hypothesis

`.VAR` (in `src/runtime/methods_mut_dispatch.rs`, the `method == "VAR"` arm) now
returns the bound value itself — rather than a synthesized `Scalar` — when the
target name is recorded in the interpreter's readonly map with
`ReadonlyKind::Immutable` or `ReadonlyKind::ImmutableValue`. That is exactly the
"this name has no container" property, and it is already recorded for the topic:
`vm_for_loop_body.rs`, `vm_for_loop_intrange.rs`, `vm_for_loop_lazy.rs` and
`vm_given_when_ops.rs` all mark `"_"` with `ReadonlyKind::Immutable` when the
topic source is immutable (that is what makes `given 5 { $_ = 6 }` throw
`X::AdHoc` / "Cannot assign to an immutable value", which mutsu gets right).

So the marking is there but the `.VAR` lookup does not see it. The likely
culprits, in order:

- the `.VAR` arm returns earlier through the `var_meta_value(target_var)` cache
  (a previously built meta instance for `$_`), before the readonly-kind probe is
  reached;
- `target_var` for the topic is not the `"_"` key the readonly map uses (the
  probe trims a leading `$`, but the topic may arrive as something else).

`rust-gdb -batch` breaking on the `method == "VAR"` arm and printing
`target_var` answers which, with no rebuild.

## Affected files

- `src/runtime/methods_mut_dispatch.rs` — the `method == "VAR"` arm.
- Pin the fix in `t/readonly-assign-exception-taxonomy.t`, whose `.VAR` section
  already covers the neighbouring cases (and must keep passing under real `raku`).
