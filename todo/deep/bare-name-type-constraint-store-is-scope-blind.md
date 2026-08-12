# Bare-name type-constraint store is scope-blind (constraint leaks across frames)

`Interpreter::var_type_constraints` is a single global `HashMap<String,
String>` keyed by BARE variable name. Every typed `my` declaration writes it
(`set_var_type_constraint`, `src/runtime/runtime_var_meta.rs`), and it is
never frame-scoped, so a typed lexical in ANY frame leaks its constraint
onto every same-named variable that is assigned later — across routines,
even across compunits.

## The shapes seen so far

1. **Fixed case-by-case:** PR #6337 (2026-08-12) made an untyped
   expression-position decl CLEAR a stale same-named constraint at
   declaration time. Typed params already avoid the global map entirely
   (`bind_param_type_constraint` writes env-scoped `__mutsu_type::` only,
   restored with the frame).
2. **Open (Text::CSV `t/66_formula.t` line 129):** clearing at declaration
   is not enough when the leak happens AFTER the victim's declaration. The
   test script declares untyped `my $e;` (clears fine), then calls
   `$csv.string`, and Text/CSV.rakumod:921 `my Str $e = $!esc;` inside
   `method string` re-registers bare "e" → Str in the global map. The
   module frame exits (its env-scoped entry dies with it) but the global
   entry survives, so the script's next `CATCH { default { $e = $_ } }`
   dies "Type check failed in assignment to $e; expected Str but got Any".
   Repro:

   ```raku
   class C { method m () { my Str $s = "x"; } }
   my $s;
   C.m;
   $s = 42;   # mutsu: type check failed; raku: fine
   ```

## Why the quick fixes don't compose

- Making routine-body typed `my` env-only (like params) breaks the global
  fallback that `bind_param_type_constraint`'s comment documents (EVAL'd
  re-assignment to a subset-typed lexical), and closures that outlive the
  frame lose enforcement either way.
- Restore-on-return bookkeeping (save/restore the global entry per frame)
  has the same closure problem in the other direction: a closure captured
  from the dead frame should STILL enforce its lexical's constraint.

## The sound architecture

Rakudo attaches the constraint to the Scalar CONTAINER, not to a name: a
container created by `my Str $e` carries `of Str` wherever it flows —
closures keep enforcement, frames leak nothing, names never collide. mutsu
already has per-container metadata precedents (ArrayData/HashData type
metadata, ADR-0013 GcBox cells). The fix direction is to carry scalar
constraints on the container cell (ContainerRef/GcBox layer) and make
assignment check the TARGET CONTAINER's constraint instead of consulting a
name-keyed side table; the name-keyed store then shrinks to compile-time /
EVAL bridging only.

Blocks: Text::CSV `t/66_formula.t` (test after 72, line 129; the suite is
otherwise expected green through that file). Related pins:
`t/expr-decl-stale-type-constraint.t` (#6337).
