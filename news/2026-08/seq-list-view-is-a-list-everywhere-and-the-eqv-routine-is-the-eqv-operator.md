# The `.cache` List view is a `List` everywhere, and `&infix:<eqv>` is the `eqv` operator

Three of the whitelisted files still regressing under `MUTSU_REAL_TEST=1`
(`todo/deep/vendor-real-test-module.md`) formed the campaign's "Seq/List"
cluster: `roast/S32-list/tail.t` (test 57), `roast/S32-list/seq.t` (test 34's
"methods still throw when Seq is NOT cached" subtest) and `roast/S16-io/words.t`
(tests 1, 2, 5, 6). They are **two** root causes, not one, but both live in
`infix:<eqv>` — which is exactly why only the real `Test.rakumod` sees them: its
`is-deeply` narrows `Seq` arguments with `.cache` and then compares with `eqv`,
and its `cmp-ok` reaches an operator only through the *routine* form
`&CALLER::LEXICAL::("infix:<$op>")`. The native Rust `is-deeply`/`cmp-ok` do
neither.

## Root cause 1: the `SeqView::List` handle was only a `List` by name

[ADR-0038](../../docs/adr/0038-seq-cache-returns-a-list-and-the-seq-list-view-is-a-property-of-the-value.md)
gave `.cache` on a not-yet-reified `Seq` a second handle over the same body,
tagged `SeqView::List`, and taught `value_type_name` to read that tag. That fixed
the stack overflow the ADR was written for — `is-deeply`'s narrowing terminates
because the narrowed argument no longer binds `Seq:D`. But the tag was read by
`value_type_name` **and nothing else**, so the value's own representation stayed
`ValueView::Seq`, and the two sites that ask a *structural* question about a
value's Raku type never saw it:

```raku
my $d = Seq.new(class :: does Iterator {
    has @!stuff = <a b c>;
    method pull-one { @!stuff and return @!stuff.shift; IterationEnd }
}.new);
my $c = $d.cache;
say $c.^name;                 # List          (right, since ADR-0038)
say $c.raku;                  # ("a","b","c").Seq   <- raku: ("a","b","c")
say $c eqv ('a','b','c');     # False               <- raku: True
say $c eqv <a b c>.Seq;       # True                <- raku: False
```

`eqv` is type-strict by definition, so a value that Raku calls a `List` must
compare as one; `.raku` renders the type, so it must render one. Both now
normalise through a single new helper, `Value::seq_list_view_as_list` — the same
"one oracle" shape ADR-0038 §2 applied to the type *name*, applied to the value's
*representation*. `reify_or_consume_eqv_operand` learned it too: when it has to
take a body it now rebuilds a `List`-view handle as a `List`, not as a plain
`Seq`.

That is `tail.t` 57 and all four `words.t` failures: both are
`is-deeply <deferred Seq>, <something>`, whose first narrowing step produced a
`Seq`-shaped value that then compared unequal to the `List` on the other side.

## Root cause 2: the routine form of `eqv` was a different operator

`a eqv b` compiles to `OpCode::Eqv`, whose handler carries the whole contract:
the `X::Cannot::Lazy` rules for two lazy iterables, `Proxy` element FETCH, the
same-`Seq` identity fast path, and the Seq reify/consume protocol that raises
`X::Seq::Consumed`. `&infix:<eqv>($a, $b)` went somewhere else entirely — it fell
through `call_infix_routine` to the pure `apply_reduction_op` fold, which is just
`Value::eqv`. So:

```raku
(my $s1 = (1,2,3).Seq.slice(0,1,2)).sink;   # consume it
(my $s2 = (3,4,5).Seq.slice(0,1,2)).sink;
$s1 eqv $s2;                # throws X::Seq::Consumed  (correct)
&infix:<eqv>($s1, $s2);     # answered False           (wrong)
```

`seq.t`'s `throws-like { cmp-ok $s1, 'eqv', $s2 }, X::Seq::Consumed` therefore
saw no exception at all, and the un-thrown `cmp-ok` emitted its own TAP line
inside the `throws-like` subtest ("You planned 2 tests, but ran 3").

The operator's body is now `Interpreter::eqv_values`, and `call_infix_routine`
routes `eqv` through it — the same way it already routes `~~`/`!~~` for needing
the full interpreter. The native provider's `cmp-ok` kept a hand-rolled
consumed-`Seq` check for precisely this gap; that check is now describing
behaviour the operator itself has.

There turned out to be a *third* route to `eqv` with the same defect:
`eval_reduction_operator_values`, which serves `[eqv]` and every metaop
(`Zeqv`, `Xeqv`, `>>eqv<<`). It too answered from the static
`apply_reduction_op` table, so `[eqv] $consumed1, $consumed2` said `False`
where raku throws. It gets the same one-line redirect, alongside the `=~=` and
range-operator arms already there for exactly this reason ("the static table
cannot host an operator that needs the interpreter").

## The consequence that had to be chased: `unique`/`repeated` must cache their `:as` needles

Making the routine form consume `Seq` operands (as raku does) exposed a
pre-existing bug that the non-consuming routine had been hiding.
`roast/S32-list/unique.t`'s last test is literally named *"Seq as the result of
an `:as` caches the Seq"*:

```raku
[[1], ['1'], [4]].unique(:as(*.map(&[~])), :with(&[eqv]))
```

Each `:as` result is a `Seq`, and `unique` compares every needle against every
needle seen so far — so with a genuinely consuming `&[eqv]` the second use of a
needle dies. Rakudo caches the needle; mutsu did not, and only got away with it
because its `&[eqv]` was inert. `unique` and `repeated` now `.cache` a
`Seq`-valued `:as` needle (`.cache` does not force a deferred body, so an
infinite `:as` result stays lazy).

`squish` deliberately does **not** get the same treatment, and that asymmetry is
measured rather than guessed: with a `Seq`-valued `:as` and `:with(&[eqv])`,
`raku`'s `unique` and `repeated` both answer while its `squish` throws
`X::Seq::Consumed` — `squish` keeps only the previous needle, which is used once
as the right operand and again as the left one, and rakudo never cached it there.
mutsu now reproduces all three exactly.

## Measured, file by file (release build, `scripts/run-roast-test.sh`)

| file | real Test before | real Test after | native before | native after |
| --- | --- | --- | --- | --- |
| `roast/S32-list/tail.t` | 1 failure (#57) | **PASS** | PASS | PASS |
| `roast/S32-list/seq.t` | 1 failure (#34) | **PASS** | PASS | PASS |
| `roast/S16-io/words.t` | 4 failures (#1, #2, #5, #6) | **PASS** | PASS | PASS |

Three named files off the campaign's 2026-08-29 re-measured 40-file list, on a
file set disjoint from the sibling slices landing the same day.

Pin: `t/seq-cache-list-view-and-eqv-routine.t` — 38 assertions, green under real
`raku` as well as under mutsu, covering the `.cache` List view in all five facets
(`.^name`, smartmatch, `eqv` against List/Seq/Array, `.raku`, `.elems`), the
original handle keeping its own `Seq` type, plain type-strict `eqv`, the routine
form of `eqv` (agreement, type strictness, the List view, and `X::Seq::Consumed`
from operator, routine, `cmp-ok`, `[eqv]` and `Zeqv` alike), and `is-deeply`'s
narrowing end to end.

Verification: `make test` green; the three files green under **both** providers;
a 444-file native roast sweep across `S32-list`, `S32-array`, `S16-io`, `S32-io`,
`S02-types`, `S04-statements`, `S07-*`, `S03-operators` and `integration` green
(it is what caught the `unique.t` consequence above); and
`scripts/battery-testsuite.sh` on a release build unchanged.

## Note for the rest of the residue

ADR-0038's "one oracle" rule was written about the type *name*. The lesson here
is that a value carrying a type tag needs the same discipline everywhere its type
is *observable* — `eqv` and `.raku` are as much type oracles as `.^name` is. And
the second bug is the operator-vs-routine version of the trap the campaign has
already recorded twice for fast paths: when two code paths implement one
operation, the intercepted one is reliably the poorer of the two, and the real
`Test.rakumod` is very good at finding whichever one mutsu did not think of as
"the" implementation.
