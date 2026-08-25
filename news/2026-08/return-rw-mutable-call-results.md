# `return-rw` call results are mutable

`return-rw` hands the *caller* the container a variable is bound to, rather than a
decontainerized copy, so the call result can be assigned to and mutated in place. mutsu already
implemented the hard half of that — a `return-rw` operand that is a subscript or an attribute
compiles in container-producing mode (`Compiler::compile_return_rw_arg`) and yields a shared
cell the assignment site writes through — but the everyday spellings all failed:

```raku
sub s() { my $a = 41; return-rw $a }
say ++s();                                  # raku: 42   mutsu: "the parameter requires mutable arguments"

my $v = 1; sub f() is rw { return-rw $v }
f() = 5;                                    # raku: $v is 5   mutsu: "Unknown call: return-rw"
```

The original ticket guessed this was a lost rw flag on the return path. It was not: it was four
independent gaps, each in a different layer.

## What was fixed

**1. Assignment through a `return-rw` of a bare lexical (`f() = 5`).** A `return-rw` operand
that is a plain variable still compiles to a decontainerized read, so the assignment falls back
to the caller-side tail re-interpretation in `assign_rw_target_expr`. That function knew how to
assign through `$v`, `@a[0]` and the `$x.return-rw` method spelling, but had no arm for the
`Expr::Call { name: "return-rw" }` shape the parser actually produces — so it fell into the
generic named-call arm and died with `Unknown call: return-rw`. It now unwraps the `return-rw`
wrapper and assigns through its operand.

**2. `is_explicit_return_rw_target` was too narrow.** It only recognised `return-rw` of an
`Expr::Var`, so `sub f() { return-rw Proxy.new(...) }` and other non-variable operands were
judged non-rw and rejected with "sub is not rw" unless the routine also carried `is rw`. In
Rakudo `return-rw` alone makes the result assignable regardless of the trait, for any operand;
the predicate now accepts any single-operand `return-rw`.

**3. `++` / `--` on a call result.** No compiler arm existed for `Expr::Call` under prefix or
postfix inc/dec, so `++f()` fell through to `__mutsu_incdec_nomatch` — and this was broken for
*every* rw routine, `return-rw` or not (`sub f() is rw { $v }; ++f()` failed identically). The
compiler cannot decide rw-ness at the use site, because the routine may be declared later in
the file, so the four forms now compile to `__mutsu_incdec_named_sub_lvalue`, which resolves the
routine at runtime (`src/runtime/incdec_rw_sub.rs`): it reads through the returned container,
applies `.succ`/`.pred` via the existing `increment_value_smart` helpers, and writes back
through `assign_named_sub_lvalue_with_values` — the very path `f() = v` uses. When the routine
turns out not to be rw-capable it raises the same `X::Multi::NoMatch` as before, so the
diagnostic for `++non_rw_sub()` is unchanged.

**4. `f() += 1` and friends.** The compound-assignment lowering
(`build_compound_assign_expr`) had arms for variables, subscripts and method calls but not for
a plain call, so `f() += 1` compiled to an unconditional `__mutsu_assignment_ro` and every
`op=` through an rw routine died with "Cannot modify an immutable value". It now lowers to
`__mutsu_assign_named_sub_lvalue`, reusing the same runtime lvalue path.

Two smaller arity bugs were fixed alongside: `return-rw $a, $b` returned only `$a` (the builtin
took `args.first()`; it now builds the list, as `return` does), and a bare `return-rw` with no
operand parsed as an inert bareword — it stringified to `"return-rw"` and let the rest of the
routine keep running, instead of returning `Nil` and exiting.

## The plain-`return` contrast

The point of `return-rw` is that plain `return` *decontainerizes*, even inside an `is rw`
routine — Rakudo rejects `sub f() is rw { return $v }; f() = 5` with "Cannot assign to a
readonly variable or a value". mutsu accepted it, because `rw_sub_target_expr` treated
`Stmt::Return(e)` and `Stmt::Expr(e)` as the same lvalue tail. A plain `return` tail now stops
the walk and yields `None` (stopping matters: falling through to an earlier statement would
make some unrelated expression the assignment target). The whole corpus contained no `is
rw`/`is raw` routine with a plain `return` tail, so this tightening had no blast radius.

## Verification

`t/return-rw.t` pins all of it — 28 assertions covering assignment, `++`/`--` in both
positions, `op=`, array elements, private attributes, `Proxy`, the routine's own `\c` parameter,
both arity edge cases, and the plain-`return`/non-rw shapes that must **not** be writable. It
passes unmodified under real `raku` as well as mutsu, so it is a spec test, not a mutsu-shaped
one. `roast/S06-advanced/wrap.t`, `S12-attributes/mutators.t`, `S02-types/mu.t`,
`S12-methods/lvalue.t` and `S06-routine-modifiers/proxy.t` — the whitelisted roast files that
exercise `return-rw` — all still pass, as do the S06/S12 families around them.

## Not fixed

Two shapes still need the container to survive as a first-class *value* rather than being
reconstructed at the assignment site: binding a returned scalar container (`my $r := f(); $r =
5`) and `return-rw` of several values (`(f())[0] = 9`). Both are recorded, with repros and the
reason they are big, in `todo/deep/return-rw-scalar-and-list-containers.md`.
