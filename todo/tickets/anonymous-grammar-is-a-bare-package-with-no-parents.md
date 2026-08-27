# An anonymous grammar registers as a bare package, so it relates to no type

`my grammar { ... }` / `grammar { ... }` used as an *expression* is parsed by
`anon_grammar_expr` (`src/parser/primary/misc/anon_decl.rs`), which emits a
`Stmt::Package { kind: PackageKind::Grammar }`. That statement registers a
**package**, not a class — it has no `parents` field at all, so unlike the
statement declarators (`grammar_module.rs:196`, `package_decl.rs:499`, which both
push `"Grammar"` when there is no `is` clause) the anonymous grammar never gets
the implicit `Grammar` parent.

Consequences, all measured 2026-08-27 against the current build:

```raku
my $g = my grammar { token TOP { \d+ } };
say $g.^name;        # <anon|1>          (raku: <anon|1>  -- fine)
say $g ~~ Grammar;   # False             (raku: True)
say $g.^parents;     # dies: No such method 'parents' for invocant of type
                     #       'Perl6::Metamodel::GrammarHOW'
say $g.^mro;         # dies the same way
```

The anonymous grammar's type object is a `CustomType` carrying a
`Perl6::Metamodel::GrammarHOW`, and that HOW implements neither `parents` nor
`mro`, so the ordinary introspection route is unavailable too.

## Why this is filed separately

Found while making a grammar's parse result report the grammar's own type
(`news/2026-08/grammar-parse-result-is-a-grammar-cursor.md`). That change had to
work around this gap: because an anon grammar's chain reaches nothing, a cursor
of one would have related to no type at all, so `dispatch_mro`,
`type_matches_value`, `.isa` and `isa_nominal_hierarchy` now assert
"a cursor IS a Match" from the value's *shape* instead of its registration. That
keeps `$cursor ~~ Match` / `.isa(Match)` / `~~ Cool` / `~~ Capture` correct, and
`t/grammar-parse-result-cursor-type.t` pins it.

But the underlying gap is untouched: the anon grammar's own **type object**
still relates to nothing (`$g ~~ Grammar` is `False`, `.^parents`/`.^mro` die),
and that is not specific to cursors.

## What the fix probably is

Route `anon_grammar_expr` through the same class registration the statement
declarators use — i.e. give the emitted declaration a `parents` list containing
`"Grammar"` (and make `GrammarHOW` answer `parents`/`mro`) — rather than
registering a bare package. Once the anon grammar is a registered class whose
MRO reaches `Grammar -> Match -> Capture -> Cool -> Any -> Mu`, the shape-based
assertions listed above become redundant belt-and-braces rather than the only
thing holding the invariant up, and `$g ~~ Grammar` / `$g.^mro` / `$g.^parents`
start working.

Worth checking the same question for `my class { ... }` and `role { ... }`
expressions, which share `anon_decl.rs` — the class path DOES carry `parents`,
so it is probably only the grammar (Package-shaped) path that is affected.

## Repro

```raku
my $g = my grammar { token TOP { \d+ } };
say $g ~~ Grammar;   # want True, get False
say $g.^parents;     # want ((Grammar)), currently dies
```
