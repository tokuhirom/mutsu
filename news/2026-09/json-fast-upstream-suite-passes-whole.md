# The real `JSON::Fast`'s upstream suite passes whole

Upstream `JSON::Fast:ver<0.20.1>:auth<zef:timo>` — the distribution 541 of the
1625 ecosystem distributions depend on, and which mutsu still answers with a
native provider — now passes **all 14 of its upstream test files, 931
assertions**, run directly against the real sources with `-I <dist>/lib`.

The previous measurement ([#8239](https://github.com/tokuhirom/mutsu/issues/8239),
the nine missing `nqp::` ops plus the `Uni`-as-codepoint-store rework) left
`t/01-parse.t` blocked on two interpreter gaps, both since fixed
([#8232](https://github.com/tokuhirom/mutsu/issues/8232) deep recursion raises
instead of aborting, [#8233](https://github.com/tokuhirom/mutsu/issues/8233)
`++$p` keeps the native reference). Re-measuring on top of those, `01-parse.t`
was green and **three other files had failures nobody had looked at**. All three
turned out to be general interpreter bugs with nothing to do with JSON, and this
is what they were.

## A brace was classified by what followed it, not by what was inside it

```raku
sub f($o, @k) { $o }
say f({ "a" => 1 }, <x y>).^name;   # rakudo: Hash    mutsu: Block
say f({ "a" => 1 }).^name;          # rakudo: Hash    mutsu: Hash
```

The same braces, two answers. `identifier_call`'s `name { block }, args` shape —
what makes `map { $_ * 2 }, @list` work — claimed any brace that was followed by
a comma, without ever asking whether the brace was a hash composer. Raku decides
that purely on what is *between* the braces; what follows the `}` never enters
into it.

The classification `block_or_hash_expr` already performs is now exposed as
`braces_are_hash_composer`, and the listop shape asks it before claiming the
brace. One classification, two call sites — not two heuristics that can
disagree. `BEGIN { … }` keeps its block unconditionally: it is a phaser, not a
call taking a first argument.

This is what made `to-json($obj, :sorted-keys)` die with "Don't know how to
jsonify Block" in `t/08-sorted-keys.t`, where the test's helper is called as
`assert-sorted { "aaaa" => { … } }, <aaaa aaab …>, message => "…"`.

## A conditional over a native `is rw` reference lost the reference

`#8233` taught the compiler that `f(++$p)` still denotes `$p` when `$p` is one
of the enclosing routine's native `is rw` parameters. `JSON::Fast`'s comment
scanner hands the position on through a conditional instead:

```raku
nom-ws($text, $ord == -1 ?? $pos !! ++$pos)
```

Each arm of rakudo's conditional yields the native reference, so the conditional
as a whole *is* that reference — whichever arm ran. mutsu compiled it as an
ordinary expression, collapsing both arms to a value, and the callee refused it
with "expects a writable container".

The fix compiles the *choice* into argument position: the condition, then each
arm through the same argument chokepoint, so an arm spelled `++$p` still
performs its increment and a bare `$p` still binds the caller's storage. Because
each arm is compiled as an argument rather than folded to one variable, the arms
may name different parameters (`f($c ?? $p !! $q)`), which rakudo also allows
and the previous shape could not have expressed.

`t/14-comments.t` went from dying before its first assertion to 3/3.

## A punned `Rational[Int,Int]` could not say what number it was

```raku
say Rational[Int,Int].new(3, 10).Str;   # rakudo: 0.3   mutsu: Rational[Int,Int]()
```

mutsu's parametric `Rational` role (the prelude role injected for user classes
written as `does Rational[…]`) carried a numerator, a denominator, `nude` and
`Bool` — and nothing that answered what number those two attributes *denote*, so
`.Str` fell through to the type-object rendering and `.Num`/`.Int`/`.abs` were
not there at all. `JSON::Fast`'s `Rational` branch stringifies the value, so
`to-json` emitted the literal text `Rational[Int,Int]()` inside its JSON and
`t/04-roundtrip.t`'s round trip then died parsing it.

The role now derives its whole numeric surface from the one division —
`Rat`, `Numeric`, `Num`, `Bridge`, `Int`, `Str`, `abs`, `floor`, `ceiling` —
which also gives `+` and `<` something to coerce through (`$r + 1` was `1`,
rakudo's `1.3`). Deriving `.Int` from the Rat rather than from `numerator div
denominator` is what makes it truncate toward zero instead of flooring, matching
rakudo for a negative value.

## Pins

- `t/collections/hash/hash-composer-listop-argument.t` (14 assertions) — both
  spellings, the block shapes that must stay blocks (`map`/`grep`/`sort`, a
  placeholder body, a block whose body builds a hash).
- `t/routines/signature/rw-param-incdec-arg-container.t` — 5 new conditional
  cases on top of the 11 from #8233, including "the untaken arm never
  increments" and the two-parameter conditional.
- `t/oo/role/rational-role.t` — 13 new assertions across the numeric surface.

Every expectation was measured against rakudo first.

## What this does not do

It does not vendor the distribution. Steps 3–4 of
[#8226](https://github.com/tokuhirom/mutsu/issues/8226) — `modules/JSON-Fast/`
plus deleting `src/runtime/json.rs` and `src/vm/vm_native_json.rs` — are the
next slice, and are what actually retires the last unjustified ADR-0096 §D4
rung-3 entry. This is the measurement that says they can go ahead.
