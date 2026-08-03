# Compile errors that name no exception class at all

`news/2026-08/typed-exception-class-from-the-message-convention.md` made the
`"X::Type: text"` message convention real, so an error that *spells* its class
now presents it. These nine do not spell one — they raise a bare
`Confused. parse error at …` (or an unrelated class) where raku raises a
specific one, so `throws-like` cannot match and a typed `CATCH` cannot dispatch.

Found by the full Test-vendoring sweep (`todo/tickets/vendor-real-test-module.md`);
each is a file that `raku` passes and mutsu fails only under the real `Test`
module. Reproduce one with:

```
$ raku  -e 'try { EVAL q{sub f() returns { }} }; say $!.^name'
$ mutsu -e 'try { EVAL q{sub f() returns { }} }; say $!.^name'
```

**Re-measured 2026-08-03: six of the seven now pass** under `MUTSU_REAL_TEST=1`
(`t/return-constraint-malformed.t`, `t/name-null.t`, `t/radix-literals.t`,
`t/unicode-identifiers.t`, `t/modifier-cond-ending-in-block.t`,
`t/block-lexical-scope.t`) — later exception-class work closed them without the
individual passes this table anticipated. Only `t/out-of-range-scalar-index.t`
still fails, and not for the reason listed: it reaches `X::OutOfRange` correctly
but the class has no `.comment` attribute, and rakudo's `DateTime` range errors
are `X::Temporal::OutOfRange` with `.what`/`.got`/`.range`/`.comment`. That is
the same shape as `news/2026-08/typed-exceptions-carry-their-attributes.md`, so
what is left here is an attribute pass, not a classing pass.

| file | first failing assertion | class raku raises |
| --- | --- | --- |
| ~~`t/bind-to-whatever-index.t`~~ | ~~binding `[*-1]` of an empty array~~ | **done** — `news/2026-08/bind-slice-is-a-real-exception-class.md` |
| ~~`t/indexed-bind-in-expression.t`~~ | ~~a Whatever-index bind~~ | **done** — same |
| `t/return-constraint-malformed.t` | a malformed return constraint | `X::Syntax::Malformed` |
| `t/name-null.t` | a type name with a null component | `X::Syntax::Name::Null` |
| `t/radix-literals.t` | no numerals in an octal literal | `X::Syntax::Confused` |
| `t/unicode-identifiers.t` | a non-ASCII digit starting an identifier | `X::Syntax::Variable::Numeric` |
| `t/modifier-cond-ending-in-block.t` | a non-block condition followed by a bare statement | `X::Syntax::Confused` |
| `t/out-of-range-scalar-index.t` | a scalar string index out of range | `X::OutOfRange` |
| `t/block-lexical-scope.t` | chained `our`/`my` in a block does not leak | `X::Undeclared::Symbols` |

The fix per case is to raise the error with `RuntimeError::typed_msg` (or one of
the dedicated constructors in `src/value/error_typed.rs`) instead of a bare
message. They are independent of each other, so this is a bag of small slices rather than
one change. The right-hand column is the class the file's own `throws-like`
names, and `raku` passes every one of these files, so it is the class raku
actually raises — but check the exact message text against `raku` before
writing one, since these constructors carry more than a class name
(`X::Bind::Slice` has `.type`, `X::OutOfRange` has `.range`/`.got`).
