# `use Module a, b, c` passes every argument to `sub EXPORT`

A `use` line may carry a comma-separated argument list, and each element is a
separate positional argument to the module's `sub EXPORT`:

```raku
use RakudoPrereq v2021.04, 'Your Raku is way too old, bruh!', 'rakudo-only';
```

mutsu parsed only the **first** argument. `expression()` stops at a comma (the
list op lives at statement level), so `use Foo 1, 2, 3` became `use Foo 1`
followed by two bare constants in sink context — the module's `sub EXPORT` saw
one argument, and the user got a spurious
`Useless use of constant integer 2 in sink context` warning.

The `use` parser now collects the whole comma list and hands it on as an
`ArrayLiteral`, which the compiler already flattens into positional `use`
arguments (the same path `use Foo <a b c>` takes). A trailing comma ends the
list.

The same gap broke **`use lib` with more than one path**. `use lib "a", "b"`
dropped the second path at parse time, and `use lib <a b>` was worse: the whole
word list reached the `UseLibPath` opcode as one value and was stringified into
the single bogus repository spec `"a b"`. Both parse-time and run-time handling
now treat the argument as a list of specs and register each one; a runtime list
(`use lib @paths`) works for the same reason. An empty spec is still rejected
with `X::LibEmpty`.

Found while triaging `TODO_dist` ticket T-046 (RakudoPrereq), whose whole API is
the arguments on the `use` line.

Pin: `t/use-multiple-export-args.t` (+ `t/lib/UseArgsFixture.rakumod`), passing
under both mutsu and raku.
