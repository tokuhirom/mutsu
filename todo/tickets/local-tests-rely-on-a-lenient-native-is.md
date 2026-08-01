# Some `t/` files assert against mutsu's lenient native `is`, not against Raku

Found while re-running the Test-vendoring bulk sweep
(`todo/tickets/vendor-real-test-module.md`). These are **test-file** bugs, not
interpreter bugs: rakudo's real `Test.rakumod` fails them, and so does `raku`
itself, because mutsu's native `is` stringifies its arguments more eagerly than
Raku's does.

## The shapes

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

**`Empty` compared against `Nil`.** `andthen` / `notandthen` yield `Empty` — an
empty `Slip` — when they skip their RHS, and so does a routine whose body ends in
a statement-modifier `if` that does not fire. `is $x, Nil` passes natively and
fails under the real module *and* under `raku`; `is-deeply $x, Empty` is the
assertion that holds.

## Corrected (2026-08-01)

`news/2026-08/test-files-asserted-against-a-lenient-is.md` — 19 files, 40
assertions, each verified three ways (mutsu's native provider, the aliased
upstream `Test.rakumod`, and `raku`):

- 35 `is <expr>.WHAT, '(Type)'` assertions across 15 files became
  `is <expr>.WHAT.gist, ...`.
- `t/lock.t` wanted a *qualified* name, which `.gist` does not give (`.gist` of
  `Lock::Async` is `(Async)` in raku too), so it asks `.^name` instead.
- 4 `is …, Nil` assertions across 4 files became `is-deeply …, Empty`. One of
  them exposed a real compiler bug — `notandthen` loaded `Nil` instead of the
  empty `Slip` its `andthen` sibling loads — which is fixed in the same change.

## A third shape: `lives-ok` takes a `Callable`

`t/variable-traits.t` passed a `Str` to `lives-ok`. The string form is
`eval-lives-ok`; raku rejects the call at compile time (*Calling lives-ok(Str,
Str) will never work*), and mutsu's native provider accepted it. It was the only
such call in the whole of `t/` — corrected in
`news/2026-08/pod-begin-at-end-of-input.md`, which also fixed
`t/pod-begin-without-identifier.t` for asserting that a mid-line `=begin` is a
Pod directive (raku reads it as an infix `=` in term position; a Pod directive
has to start a line).

## Still open

Six files from the sweep's "raku fails it too" bucket are **not** this problem
and need individual triage. In each of them `raku` fails for a reason unrelated
to `is`'s leniency — mutsu-specific syntax it cannot parse, or a module it
cannot find — so the `raku` verdict says nothing about the assertion style and
each has to be read on its own:

| file | first failure under the real module |
| --- | --- |
| `begin-phaser-begintime.t` | `right exception type (X::AdHoc)` for a die in `INIT` |
| `listop-arg-loose-logical-precedence.t` | `orelse` does not short-circuit on a true listop result (`raku` passes this one) |
| `method-private-errors.t` | `.calling-package` of `X::Method::Private::Permission` |
| `placeholder-named-in-method-do.t` | `%_` in a mainline `do {}` is not `X::Placeholder` |
| `use-version-short-adverb.t` | `Test::Util::ServerPort` cannot be `use`-d with a `:v<>` adverb |
| `vm-panic-boundary.t` | a VM panic escapes as a Rust panic under the real module |

`t/variable-traits.t` is out of this bucket but still red under the real module,
for a cause of its own:
`todo/tickets/user-trait-mod-multi-shadows-builtin-traits.md`.

Re-run `tmp/sweep-full.sh` after each to re-measure.
