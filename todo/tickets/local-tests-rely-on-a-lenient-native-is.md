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

The cheapest way to enumerate the rest is to re-run the sweep over the full set
rather than the 1-in-9 sample (`tmp/sweep.sh 1`, see the vendoring ticket).
