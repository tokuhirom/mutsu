# An EVAL'd unit does not inherit the caller's `fatal`

`fatal` is lexical to a compilation unit, and `EVAL` compiles a fresh one — so a
caller's `use fatal`, or the implicit one a `try` block turns on, does not reach
the snippet. mutsu let it through, which made every `try EVAL '…'` fatal:

```raku
try EVAL 'my $x = Failure.new; 1';                # rakudo: 1   mutsu: died
try EVAL 'sub f { fail "z" }; my $r = f(); 1';    # rakudo: 1   mutsu: died
```

Under `fatal`, assigning an unhandled `Failure` to a variable throws
(`exec_set_local_op_inner` gates exactly that on `self.fatal_mode`); without it
the `Failure` simply sits in the variable. `eval_eval_string` saved and restored
`fatal_mode` around the snippet — so a snippet's own `use fatal` correctly stopped
at the EVAL boundary — but never *cleared* it on the way in, so the caller's
setting stayed live inside.

The measured rule, re-derived against rakudo across the whole matrix:

| snippet | outer `use fatal`? | rakudo |
| --- | --- | --- |
| `my $x = Failure.new; 1` | no | `1` |
| `my $x = Failure.new; 1` | **yes** | `1` |
| `sub f { fail "z" }; my $r = f(); 1` | either | `1` |
| `use fatal; my $x = Failure.new; 1` | either | **dies** |
| `Failure.new` | either | **dies** |
| `my $x = Failure.new` | either | **dies** |

Only the snippet's own `use fatal` matters. The last two rows are not about
`fatal` at all: the `Failure` is the EVAL's *return value* and is thrown when the
caller sinks it — which is why the fix must clear the flag rather than suppress
the throw.

## The comment that argued the other way was wrong

The code carried a comment claiming the caller's `fatal` is "a runtime
dynamic-scope check the EVAL'd unit legitimately inherits", citing
`use fatal; try { EVAL q["bar"[5]] }` reporting `X::OutOfRange`. That evidence
proves nothing: an out-of-range subscript throws there **with or without**
`fatal` — measured both ways. A test whose outcome does not change with the
variable under test cannot be evidence about it.

## What it freed

`roast/S02-names/is_default.t` passes under both providers. Its assertion is
`eval-lives-ok 'my $a is default(Failure.new); 1'`, and under
`MUTSU_REAL_TEST=1` the real `Test` module's `eval-lives-ok` really EVALs the
string from inside a `try`, so the implicit `fatal` reached the snippet and the
`Failure` used as a default value threw. mutsu's native provider does not take
that path, which is why the file passed there — the ordinary shape of this
campaign's residue (`todo/deep/vendor-real-test-module.md`).

Pin: `t/eval-unit-does-not-inherit-fatal.t`, 14 assertions covering both
directions (the caller's `fatal` not reaching in, the snippet's not leaking out),
`try`'s implicit `fatal` as well as an explicit `use fatal`, and the two cases
that must still throw because the `Failure` is the EVAL's value — green under
real `raku` unchanged.
