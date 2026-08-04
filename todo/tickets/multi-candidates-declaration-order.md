# `&foo.candidates` is not in declaration order

`Routine.candidates` must return a multi's candidates in declaration order
(Rakudo does). mutsu returns them in an order that depends on hash bucket layout
and, worse, is unstable against unrelated statements elsewhere in the file.

## Minimal repro

```raku
sub twice($n) { $n * 2 }
sub add($a, $b = 5) { $a + $b }
sub named(:$x, :$y = 2) { "$x-$y" }
sub empty() { 'no args' }
multi mm(Int $x) { "int $x" }
multi mm(Str $s) { "str $s" }
multi mm(Rat $r) { "rat $r" }
for &mm.candidates -> $c { say $c.signature.gist }
```

raku prints `(Int $x)`, `(Str $s)`, `(Rat $r)`; mutsu prints `(Int $x)`,
`(Rat $r)`, `(Str $s)`.

Because the order is wrong, calling a candidate positionally picks the wrong one:

```raku
use Test;
plan 1;
sub empty() { 'no args' }
is &empty.(), 'no args', 'e1';
multi mm(Int $x) { "int $x" }
multi mm(Str $s) { "str $s" }
my &m = &mm;
say m(3);
say m('a');
say &mm.candidates[0].(7);   # dies: expected Str, got Int
```

Dropping any of the three statements before the multi declarations makes it pass,
which is the tell that the order is incidental rather than declared.

## Root cause

`Interpreter::routine_candidate_subs` (`src/runtime/methods_signature_candidates.rs`)
scans `registry.functions` for keys matching the routine name. Multi candidates
are stored under mangled keys (`GLOBAL::mm/1:Int`), so the scan visits them in
`HashMap` bucket order.

Sorting the scan result by `FunctionDef::decl_order` — the registration stamp that
already exists for exactly this "first declared wins" question — fixes the simple
case but not the one above. Instrumenting the registration sites shows `mm` is
registered **four times** for two candidates: the forward-declaration/hoist pass
registers both, then source-order registration registers both again, and in the
second pass the two candidates can be stamped in the *opposite* order
(`Str` got `decl_order` 3 and `Int` got 4 in the failing repro, while the same
file without the three leading statements stamped `Int` 4 and `Str` 5). So the
final stamps do not reflect source order either.

The fix therefore has to make registration order deterministic — most likely by
not re-stamping `decl_order` when a registration replaces an existing candidate
under the same key, or by keeping the source-order pass's per-candidate order
stable — before `.candidates` can sort by it.

## Affected files

- `src/runtime/methods_signature_candidates.rs` — `routine_candidate_subs`, the
  reader. It also assigns `__mutsu_multi_index` positionally, so the doc-comment
  lookup that reads that index inherits the wrong order.
- `src/runtime/registration_sub.rs` — the `decl_order:
  crate::runtime::resolution::next_decl_order()` stamps (five sites) and the
  hoist/source-order double registration.

## Why it is not a one-liner

The reader-side sort is trivial; making the stamp trustworthy means understanding
why a multi is registered twice and why the second pass can reverse the pair.
That is the same forward-declaration/source-order registration machinery ADR-0019
phase C/D is rewriting, so it may be cheaper to fix once declarations register
from a plan rather than from two AST passes.
