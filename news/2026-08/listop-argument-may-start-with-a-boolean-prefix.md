# A paren-less call argument may start with `!` or `?`

A no-paren call whose first argument starts with a boolean prefix operator lost
its entire argument list when the sub was declared *later* in the file:

```raku
sub caller-first { later !1, 2 }
sub later($x, $y) { say "$x $y" }
caller-first;   # raku: "False 2"
                # mutsu: Calling later() ... Too few positionals passed; got 0
```

A forward reference does not satisfy `is_user_sub` at the point the call is
parsed, so the parse falls through to the "does the next token start a term?"
gate. That gate listed sigils, digits, quotes, `(`, hyper-prefix operators and
slips — but not a boolean prefix. `later !1, 2` therefore parsed as *two*
statements: a bare `later` term, and an unrelated `(!1, 2)` list. The same call
written after the declaration always worked, which is why this survived.

`!` and `?` are now accepted when the very next byte is a sigil, `(`, a quote or
a digit. That is safe where a bare `+`/`-` is not (`pi - 1` must stay a
subtraction): every Raku infix beginning with `!` or `?` continues with an
operator character (`!=`, `!==`, `!~~`, `!=:=`, `??`, `?|`, `?&`, `?^`), and the
negation-metaoperator forms (`!eq`, `!eqv`, `!before`) continue with a letter,
so requiring a term character right after excludes all of them. Pinned by
`t/listop-bool-prefix-arg.t`, which guards the infixes too and passes under
`raku`.

Found in rakudo's own `Test.rakumod`, where `unlike` is

```raku
my $ok := proclaim !($got ~~ $expected), $desc
```

with `proclaim` defined 200 lines further down — the third general interpreter
bug on the way to running the genuine upstream module
(`todo/tickets/vendor-real-test-module.md`).
