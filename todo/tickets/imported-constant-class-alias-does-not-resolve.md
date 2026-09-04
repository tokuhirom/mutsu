# An imported `my constant` alias for a class does not resolve to the type object

A module that gives a verbosely-named class a short alias — a very common Raku
packaging idiom — exports it as a constant:

```raku
unit module RoundedMod;
class Array::Rounded is Array is export {}
my constant Rounded is export = Array::Rounded;
```

In the consumer, mutsu resolves that imported name to the *base* class instead
of the aliased one, both as a bareword term and in the `is` trait:

```
raku  -I lib -e 'use RoundedMod; say Rounded.new(1).^name'   # RoundedMod::Array::Rounded
mutsu -I lib -e 'use RoundedMod; say Rounded.new(1).^name'   # Array

raku  -I lib -e 'use RoundedMod; my @a is Rounded = 1,2,3; say @a.^name'  # RoundedMod::Array::Rounded
mutsu -I lib -e 'use RoundedMod; my @a is Rounded = 1,2,3; say @a.^name'  # Array
```

Re-verified 2026-09-04 on `main` (b0a4fdae0) against `raku` v2026.06 with the
fixture above.

## Two separate resolution paths are involved

The gap was originally recorded (2026-08-20) as an `is`-trait-only problem,
then found on 2026-09-01 to be wider: the plain bareword also fails, so the
imported constant does not resolve to a *type object* at all, not merely in
the trait position.

- `exec_apply_var_trait_op` (`src/vm/vm_var_trait_ops.rs`) matches the trait
  name literally against `registry().classes` / `registry().roles`. That only
  succeeds when the trait names the class directly (`is Array::Rounded`, or a
  same-file `my constant`). It has no access to the `compiled_fns`/`code`-driven
  bareword resolution chain that `exec_get_bare_word_op`
  (`src/vm/vm_var_get_ops.rs`) uses, so plumbing the general resolution through
  needs its own small design pass.
- The bareword path *does* resolve a same-file `my constant` alias correctly;
  it is the cross-module (`is export`) case that answers the base class. Find
  out where the exported constant's value loses its class identity before
  designing the trait-side fix — the two may share one cause.

## Why it matters

This is the `Array::Rounded` idiom, and many "provide a nicer-named constant
for a verbosely-named class" modules follow it, so it likely affects other
dists. `Array::Rounded`'s row in `dist-test-suite-failures-batch.md` stays open
until this is resolved; the postcircumfix half of that dist's blockers was
fixed on 2026-09-04 (`news/2026-09/core-postcircumfix-subscript-routine.md`).

## Repro fixture

`lib/RoundedMod.rakumod` as shown above (one `unit module`, one exported class,
one exported `my constant` alias), plus the two one-liners.
