# A parse failure carries an `X::Syntax::` class instead of collapsing to `X::AdHoc`

`news/2026-08/typed-exception-class-from-the-message-convention.md` made the
`"X::Type: text"` message convention real, which typed every compile-time error
whose message already *spelled* its class. What it could not reach were the
errors that name no class at all — a parse failure reported as `Confused. parse
error at line 1, column 1: expected ...`, where raku raises a specific
`X::Syntax::` class. `throws-like` and a typed `CATCH { when X::Syntax::… {…} }`
both dispatch on the class, so those assertions still failed.

## The catch-all

A parse failure is a *syntax* error, and raku's catch-all for a construct it
cannot describe more precisely is `X::Syntax::Confused` — which is also what
mutsu's own message has always said. `RuntimeError`'s untyped fallback now picks
its class from the structured `code` metadata rather than from the message text:
a `ParseUnparsed` / `ParseExpected` / `ParseGeneric` error with no structured
exception and no message convention becomes `X::Syntax::Confused`; everything
else stays `X::AdHoc`, the class a bare `die "msg"` produces. Both IS-A
`Exception`, so `isa-ok $!, Exception` matches either way. The compile-time sites
that raise something more specific (`X::Undeclared::Symbols`,
`X::Comp::WheneverOutOfScope`, `X::Redeclaration::Outer`, `X::Comp::AdHoc`)
already attach a structured exception and never reach the fallback.

## The specific shapes

Three constructs raku names precisely were reaching the parser's generic
"expected ..." path, so the catch-all alone would have mistyped them:

- **A `-->` return constraint that is not last in the signature.**
  `sub f (--> Bool, Int $y)` and `sub f ($x; --> Bool; Int $y)` only produced
  `expected ')'`, because the return-constraint parser returned as soon as it had
  read the type and left the stray tail to the caller. The signature-final
  position now checks that the constraint is followed by the closing `)` and
  otherwise raises `X::Syntax::Malformed` with raku's wording, *Malformed return
  value (return constraints only allowed at the end of the signature)*. The `;`
  and `;;` multidimensional branches learned to read a `-->` at all, which is how
  the third shape above reaches that check instead of trying to parse `-->` as a
  parameter.
- **A variable name opening with a digit of a non-ASCII script.** `my $১০kinds`
  is `X::Syntax::Variable::Numeric` in raku; mutsu's check was
  `is_ascii_digit`, so only `my $0` was caught. The rule is about the digit
  property, not the encoding.
- **A variable name opening with a combining mark.** `my $̈a` is a malformed
  declarator (`X::Syntax::Malformed`, *Malformed my*), not a confusing one.
- **A null component in a bareword qualified name.** `$a::::b` and `@a::::b`
  already raised `X::Syntax::Name::Null` through `qualified_ident`, but
  `Foo::::Bar` is parsed by the bareword type-name path in `primary/ident`,
  which qualifies names of its own and fell through to a generic *identifier
  after '::'*. It now shares the same error builder.

## Effect on the Test-vendoring sweep

Measured against the unmodified upstream `Test.rakumod` under the `Test2` alias
(`todo/tickets/vendor-real-test-module.md`): of the 7 files still failing on an
exception *class*, **5 are cleared** — `modifier-cond-ending-in-block.t`,
`radix-literals.t`, `return-constraint-malformed.t`, `unicode-identifiers.t`,
`name-null.t`. The two that remain are different root causes, not missing
classes: `block-lexical-scope.t` wants `X::Undeclared::Symbols ~~ X::Undeclared`
(the unregistered-hierarchy problem of
`todo/deep/exception-class-hierarchy-is-mostly-unregistered.md`) and
`out-of-range-scalar-index.t` fails on `use fatal` inside a string-form
`throws-like`, where the code does not die at all.

Pinned by `t/parse-failure-syntax-exception-class.t`, whose 12 assertions are
green under `raku` too.
