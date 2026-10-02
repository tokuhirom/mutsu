# `$GLOBAL::n++` that creates the variable survives a nested frame

```raku
role CountR { $GLOBAL::n++ }
sub f { my class CountC does CountR { } }
f(); say $GLOBAL::n    # raku: 1   mutsu: (Any)
```

The role body ran (a `say` in it printed); only the write was lost. The same
statement was lost in the body of a class declared in a routine and in an
`EVAL` inside a routine -- anywhere it runs in a nested frame whose env is
restored on exit, which keeps only the keys the frame held on entry. `=` and
`+=` survived the same trip; `++`, `--` and the prefix forms did not. The role
was never the cause.

`SetGlobal` ends by persisting every global scalar write in `our_vars`, "so they
survive block-scope restoration". The by-name read-modify-write store that
`++`/`--` (and the fused compound assignment) share, `store_scalar_by_name_for`,
wrote only the frame's env, so a write that *created* the variable -- the
auto-vivifying `++` of an unset `$GLOBAL::n` -- vanished with the frame. It now
persists a package-qualified name (`GLOBAL::n`, `Foo::x`) in `our_vars` the same
way.

The read side needed the matching half. `++` read its operand from the env only,
so the second call of such a routine started from zero again
(`sub body { my class K { $GLOBAL::k++ } }; body(); body()` counted 1). A new
`qualified_our_var_read` gives it the `our_vars` fallback `GetGlobal` already
takes; a name that is not package-qualified is untouched.

`$GLOBAL::q = 7; $GLOBAL::q++` in one EVAL in a routine used to end at 7 (the
`=` persisted, the `++` did not); it ends at 8 now, as in rakudo.

Pinned by `t/oo/role/role-body-qualified-incdec-from-routine.t` (13 tests,
rakudo's output): the reported role shape and its unit-level twin, all four
operators, `+=` / `=`, a class body in a routine called twice, an EVAL called
twice, an `=` then `++` in one EVAL, the GLOBAL stash read, and a `Pkg::`
qualified name.

Found while checking neighbours, not fixed here: `$GLOBAL::n.push(1)` on an
unset variable inside a role body composed from a routine leaves it `Any`
(rakudo: `$[1]`), a different store path (auto-vivifying a scalar into an
array), filed as
[#10620](https://github.com/tokuhirom/mutsu/issues/10620).
