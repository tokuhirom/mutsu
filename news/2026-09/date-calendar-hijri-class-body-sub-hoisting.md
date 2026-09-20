# Date::Calendar::Hijri: a class body's own `sub` was not hoisted

Locked and worked via the ecosystem distribution roulette (board:
[tokuhirom/mutsu#7884](https://github.com/tokuhirom/mutsu/issues/7884)).
`Date::Calendar::Hijri` 0.1.0 went from `blocked_load` (the module did not
even `use`) to `red` (2/3 baseline files at parity, 1 file blocked by
separate, filed interpreter gaps — see below).

## A `sub` declared inside a class body is not hoisted to its top

Raku hoists every `sub`'s name to the top of its *enclosing scope* at compile
time, regardless of what kind of scope that is — `Compiler::hoist_sub_decls`
already does this for a compilation unit's own top level (ADR-0041 §9), which
is why a top-level statement can call a `sub` declared later in the same
file. The same hoist never happened for a *class body*'s own top level, so a
class-body statement above a `sub`'s textual position could not call it.

`Date::Calendar::Hijri.rakumod` hits this directly:

```raku
unit class Date::Calendar::Hijri ...;

my ($f0, $g0) = make-fct(     1,  1,         -1);
my ($f1, $g1) = make-fct(   325, 11,       -320);
my ($f2, $g2) = make-fct(10_631, 30, 58_442_583);

...

sub make-fct(int $a, int $b, int $c) {
  my $f = sub (int $x) { ... };
  my $g = sub (int $x) { ... };
  return $f, $g;
}
```

which mutsu rejected outright with `Unknown function: make-fct` — nothing in
the module could load.

## Fix

`ClassBodyOp::ClassSub` now carries a second, `__hoisted`-marked compiled
chunk (`hoist_chunk`) alongside its ordinary in-sequence one, mirroring the
top-level mechanism exactly: `run_class_body` runs every class-body `sub`'s
`hoist_chunk` in one pass *before* any class-body statement executes, so the
name is callable everywhere in the body regardless of textual order. The
`sub`'s own unmarked chunk still runs at its normal position and is what a
later `BEGIN`-time reference sees as "reached"
(`Interpreter::mark_hoisted_decl_reached`) — the hoist pass only registers
the routine, it never executes its body, so this is safe even for a `sub`
whose body itself is only valid once earlier statements have run.

Pinned by `t/oo/class/class-body-sub-forward-ref.t`.

## Residue: two more general bugs found, filed rather than fixed here

Bringing the module far enough to load surfaced two further, unrelated
interpreter gaps in its dependency closure and in a sibling module in the
same distribution — both filed rather than fixed in this PR, since neither
is bounded to a same-file compiler/VM slice:

- [#8869](https://github.com/tokuhirom/mutsu/issues/8869) (`todo:ticket`): a
  package `sub` inside a class body reads a class-body `my` array static as
  empty, even though the initializing statement has already run by call
  time. The equivalent case for a *method* is already correct
  (`t/oo/class/class-body-static-in-sub.t`); this is the sibling gap for the
  `sub` path. Blocks `Date::Calendar::Hijri::Names::month-abbr`, which reads
  exactly such an array (`t/03-accessors.rakutest`).
- [#8870](https://github.com/tokuhirom/mutsu/issues/8870) (`todo:deep`): a
  module that re-exports an already-imported routine via
  `OUR::{'&name'} := &name;` (the pattern `JSON::Class` and others use to
  re-export a trait like `trait_mod:<is>`) makes a same-named local `multi
  sub` in the importer falsely raise "Redeclaration of routine". Blocks
  `META6` (used by `Test::META`, and therefore `t/02-test-meta.rakutest` in
  many zef distributions, not just this one).
