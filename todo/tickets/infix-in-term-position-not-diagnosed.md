# `X::Syntax::InfixInTermPosition` is registered but nothing in the parser ever raises it

Found investigating `t/malformed-syntax-classes.t`'s regression under
`MUTSU_REAL_TEST=1` (continuing `todo/deep/vendor-real-test-module.md`).

`raku -e 'my @a = 1, => 2'` reports:

```
===SORRY!=== Error while compiling -e
Preceding context expects a term, but found infix => instead.
at -e:1
------> my @a = 1, =><HERE> 2
```

mutsu reports the generic `X::Syntax::Confused` instead, and even the
*message* is generic ("Confused. expected statement: expected right-hand
expression after ...") — this is not a case of an already-correct diagnosis
being flattened away (the usual pattern this campaign has fixed several times,
see `news/2026-08/parse-error-keeps-its-exception-class.md`); the parser
genuinely never detects "an infix operator token appeared where a term was
expected" anywhere.

`X::Syntax::InfixInTermPosition` is already registered
(`src/runtime/runtime_init.rs:2059`) and `type_constraints.rs` knows about it,
but a repo-wide grep finds no call site that actually constructs one — only a
doc comment in `src/parser/stmt/decl/my_decl_assign.rs`'s
`malformed_initializer` (which deliberately declines to convert this shape to
`X::Syntax::Malformed`, on the assumption that a lower-level parser rule would
have already produced the better diagnosis — it never does).

## What's needed

A term-parsing failure whose next token is a recognized infix operator
(`=>`, and presumably any other infix — `+`, `,` is not itself the trigger
here, it's `=>` specifically since `,` is a valid list separator) should
raise `X::Syntax::InfixInTermPosition` with rakudo's exact message shape:
`"Preceding context expects a term, but found infix $op instead."`.

## Why this wasn't attempted this session

Finding the right insertion point requires auditing the parser's term-parsing
combinators (this repo's parser uses soft/backtracking alternatives
extensively — see the `todo/deep/vendor-real-test-module.md` entries on the
`when SomeUndeclaredType` broadening attempts, reverted twice for the same
reason) for wherever "expected a term" is the terminal failure, and turning
recognized-infix-operator failures at that exact point into a fatal,
correctly-classed error without breaking any of the many OTHER "expected a
term" call sites that must stay generic. Given this repo's established
pattern (a broadened parser condition needs verification against the *full*
`t/` + roast corpus, not just the one motivating file), this is a genuine
parser feature to implement carefully, not a quick fix — a dedicated session
with the usual "verify against every file exercising the same punctuation"
discipline is the right shape for it.

## What it blocks

`t/malformed-syntax-classes.t` test 4 and `roast/S32-exceptions/misc2.t`'s
"did we throws-like X::Syntax::InfixInTermPosition?" subtest, both under
`MUTSU_REAL_TEST=1`.
