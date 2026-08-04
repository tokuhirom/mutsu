# A compound assignment as a call argument evaluates to a Pair, not the assigned value

`@r.push($x += 5)` pushes the Pair `x => 5` instead of the assigned value `5`:

```raku
my @r; my $x; @r.push($x += 5); say @r;
# raku:  [5]
# mutsu: [x => 5]
```

Wrapping defuses it (`@r.push(~($x ~= "z"))` and `@r.push(($x += 5))` — the
latter still shows the Pair, so the trigger is the AssignExpr-as-argument
shape, not parenthesization; `~(...)` works because the coercion consumes the
value). The AST is correct (`AssignExpr { name: "x", expr: Binary { … } }` as
the call arg), so the compiler or VM's argument evaluation of `AssignExpr` is
turning `name`/value into a Pair — most likely a named-argument-construction
path misfiring on an argument that is an assignment rather than a colonpair.

Pre-existing on main (2026-08-04, found while pinning the anonymous-state
per-routine-call fix — the probe `@r.push($ += 5)` hit it with the anonymous
spelling too). Statement-level `$x += 5` and `my $y = ($x += 5)` are fine.
