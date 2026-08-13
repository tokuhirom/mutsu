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

Blocks: Text::CSV `t/66_formula.t` (test after 72, line 129). Related pins:
`t/expr-decl-stale-type-constraint.t` (#6337).

## Status 2026-08-13: this is the LAST real Text::CSV suite blocker

After the `TagContainerRef` frame-leak fix (#6347) the full Text::CSV suite
(33 files, 22685 tests) is green except:

- `t/66_formula.t` — aborts after 72/72-passing tests at exactly this
  ticket's line-129 shape (`my $e;` in the script poisoned to `Str` by
  `method string`'s `my Str $e = $!esc;`). Fixing this ticket finishes the
  file.
- `t/99_meta.t` — `Unknown function: meta-ok`: needs the ecosystem
  `Test::META` module (a dist-metadata QA test), unrelated to this ticket
  and to CSV functionality.

Scoping note for the fix: mutsu scalars in locals are bare NaN-boxed values
(no Scalar container object exists unless boxed into a `ContainerRef`), so
"attach the constraint to the container" concretely means either (a) keying
enforcement by *declaration site* — a compile-time slot→constraint table on
`CompiledCode` for slot-resident scalars, plus the existing env-scoped
`__mutsu_type::` for env-resident ones — with the global name-keyed map
shrinking to EVAL bridging; or (b) boxing typed scalars into cells that
carry `of`. (a) preserves the no-boxing perf profile and matches how the
compiler already knows the constraint statically at every `my Str $x` site;
closures that capture the lexical get the constraint through the captured
cell in either design.
