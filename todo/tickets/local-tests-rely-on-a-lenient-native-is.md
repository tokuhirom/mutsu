# Some `t/` files assert against mutsu's lenient native `is`, not against Raku's

Found while re-running the Test-vendoring bulk sweep
(`todo/tickets/vendor-real-test-module.md`). These are **test-file** bugs, not
interpreter bugs: rakudo's real `Test.rakumod` fails them, and so does `raku`
itself, because mutsu's native `is` stringifies its arguments more eagerly than
Raku's does.

## The two shapes

**A type object compared against its gist spelling.** `is Point.WHAT, '(Point)'`
passes under mutsu's native provider and fails everywhere else, because Raku's
`is` compares `$got.Str` — and a type object's `.Str` is the empty string with a
warning, not its `.gist`:

```
$ raku -e 'use Test; plan 1; class Point {}; is Point.WHAT, "(Point)", "what"'
1..1
not ok 1 - what
# expected: '(Point)'
#      got: (Point)
```

`.gist` (or `.^name`, or `isa-ok`) is what these assertions actually mean.

**A lazy `Seq` compared against its reified contents.** `is $fh.lines, 'A B C'`
passes natively and gives `'(...)'` under the real module — again matching Raku,
which does not reify a lazy sequence to stringify it. `is $fh.lines.join(' '),
'A B C'` (or `is-deeply` against a list) is the assertion that survives.

## Affected files

From the 1-in-9 sample (301 of 2717 `t/` files), six regressed on this alone:

| file | failing assertions |
| --- | --- |
| `t/typed-container-what.t` | 9 |
| `t/is-lazy-io-lines.t` | 2 |
| `t/class-basic.t` | 1 |
| `t/class.t` | 1 |
| `t/enum.t` | 1 |
| `t/float-num.t` | 1 |

At that sample rate the whole suite holds roughly 50 such files. They are not
blocking anything today — they only surface when step 3 of the vendoring plan
swaps mutsu's provider for the real module — but they have to be corrected
before that swap, and each correction makes the test *more* faithful to Raku, so
none of them needs to wait for it.

## The full enumeration (2026-08-01)

The full sweep has since been run (`tmp/sweep-full.sh`, see the vendoring
ticket), and `tmp/sweep-raku-check.sh` splits its regressions by whether `raku`
also fails the file. That bucket holds **29** files, not the ~50 the sample rate
suggested:

`anon-class-what-gist.t`, `begin-phaser-begintime.t`, `class-basic.t`,
`class.t`, `complex.t`, `compound-assign-ops.t`, `cpp-constructor-syntax.t`,
`dotassign-store-and-container-topic.t`, `enum.t`, `float-num.t`, `junction.t`,
`listop-arg-loose-logical-precedence.t`, `lock.t`, `method-private-errors.t`,
`misc-builtins.t`, `native-array-decl.t`, `new-operators.t`,
`operator-adverbs.t`, `orelse-andthen-mixin-defined.t`, `pair-type.t`,
`placeholder-named-in-method-do.t`, `pod-begin-without-identifier.t`, `rat.t`,
`set.t`, `typed-container-what.t`, `use-version-short-adverb.t`,
`variable-traits.t`, `version.t`, `vm-panic-boundary.t`.

**The bucket is not purely lenient-`is`.** Four of them (`enum.t`,
`placeholder-named-in-method-do.t`, `variable-traits.t`, `version.t`) do not
even compile under `raku` — they exercise mutsu-specific syntax, so "`raku`
fails it" says nothing about the assertion style. Check each file individually
before rewriting it; the two shapes above are the ones to correct, and anything
else in the list is a different finding.
