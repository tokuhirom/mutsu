# `X::Comp::Group`: what rakudo throws when one construct draws two complaints

Classifying the roast residue under the real `Test` module by its *first* failing
assertion (`todo/tickets/vendor-real-test-module.md`) left one cluster clearly
on top: 7 files whose first loss is `right exception type (X::Comp::Group)`.
This closes 5 of them.

## The rule

rakudo's compiler does not stop at the first complaint. It accumulates *worries*
(warnings), *sorrows* (recoverable errors) and at most one *panic* (the fatal
one), and what it finally throws depends on what it collected:

| collected | thrown |
| --- | --- |
| a panic alone | that panic |
| exactly one sorrow, no worries | that sorrow |
| anything else | `X::Comp::Group` |

So `throws-like 'say', X::Comp::Group` is not roast being odd — a bare `say`
draws a *worry* ("Unsupported use of bare say…") and then a panic on the missing
argument list, which is two things. And in the same file
(`roast/S04-declarations/my-6e.t`), `my Int $a of Str` is a plain
`X::Syntax::Variable::ConflictingTypes` while `my Int $a of Str is default("z")
of Rat` is a group — because the second sorrows twice.

mutsu raises one typed exception per parse failure, so there is no accumulator to
consult. Instead, each site that reproduces a rakudo diagnosis *with a companion
complaint* now says so, through a new `PError::comp_group` built on the existing
`Value::make_comp_group`.

## The four diagnoses

| construct | what mutsu did | what it does now |
| --- | --- | --- |
| `say` / `print` / `put` with no argument | `X::Comp` carrying the advice | `X::Comp::Group`: the advice as a worry, `Argument to "say" seems to be malformed` as the panic |
| `1__0` | `X::Syntax::Confused` with a 40-alternative "expected …" dump | `X::Comp::Group`: `Only isolated underscores are allowed inside numbers` as the sorrow, `Confused` as the panic |
| `for 1.. { }`, `for 1... { }`, `for 1, {a=>1}` | `X::Syntax::Missing` | `X::Comp::Group`: `Expression needs parens to avoid gobbling block` |
| `my Int $a of Str is default("z") of Rat` | `X::Syntax::Variable::ConflictingTypes` (the first conflict only) | `X::Comp::Group` of both conflicts |

Three of the four are error-*quality* fixes as much as classification ones. The
`1__0` message in particular was the parser's generic "Confused" listing every
alternative it could have accepted — over 400 characters that told the reader
nothing — where rakudo names the actual rule in eight words.

Each change is deliberately narrow, because the neighbouring shapes are
*differently* typed in rakudo and were already right:

* `10_` stays `X::Syntax::Confused` and `_10` stays undeclared-symbol; only a run
  of two or more underscores **between digits** is the group. `1_0` is still a
  legal separator.
* `for 1..2` stays `X::Syntax::Missing` — that block is genuinely absent, not
  eaten. The gobbled-block test grew to cover `Expr::Hash` (an empty or
  key-value-looking brace) and an infix's right operand (`1 .. {}`), which is
  where the loop's block ends up when a range endpoint swallows it.
* one redundant `of` still throws `X::Syntax::Variable::ConflictingTypes` on its
  own. Getting the double form right meant making the `of` / `is`-trait parse a
  loop that collects conflicts instead of throwing on the first one — rakudo's
  accumulate-then-decide model in miniature, scoped to one declaration.

## Result

`roast/S16-io/bare-say.t`, `roast/S02-literals/underscores.t`,
`roast/S04-statements/for.t`, `roast/S04-declarations/my-6e.t` and
`roast/6.c/S04-declarations/my-6c.t` all pass under the real `Test` module.
Pin: `t/comp-group-two-complaints.t`, which also asserts the shapes that must
*not* become groups.

The two files left in the cluster: `roast/S05-metasyntax/regex.t` needed an
unterminated-regex diagnosis (done —
`news/2026-08/unterminated-regex-diagnosis.md`), and
`roast/S02-literals/quoting-unicode.t` turned out to need the `X::Comp` *role*
rather than the delimiter validation first guessed
(`todo/deep/exception-class-hierarchy-is-mostly-unregistered.md`).
