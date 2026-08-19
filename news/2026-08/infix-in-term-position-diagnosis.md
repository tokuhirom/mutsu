# `X::Syntax::InfixInTermPosition` is now raised for a recognized infix in term position

`X::Syntax::InfixInTermPosition` was registered in the exception hierarchy
(`src/runtime/runtime_init.rs`) and known to `type_constraints.rs`, but a
repo-wide grep found no call site that ever constructed one. Term-parsing
failures where the next token is a recognized infix operator — e.g.
`my @a = 1, => 2` — fell through to the generic `X::Syntax::Confused` with a
generic message instead of rakudo's specific diagnosis:

```
Preceding context expects a term, but found infix => instead.
```

The fix adds `PError::infix_in_term_position(op, input)` in
`src/parser/parse_result.rs` and seeds it into `primary()`'s (in
`src/parser/primary/mod.rs`) best-error candidate whenever the input at a
term-parse position literally starts with `=>` — the fat-comma infix can
never begin a term, so this is a deterministic lexical check, not a
heuristic. Since `=>` unambiguously cannot start a term in *any* Raku
context, checking for it is behaviorally safe wherever a term is expected.

The check is deliberately **SOFT**, not fatal. An early attempt raised the
error immediately with `return Err(...)`, which regressed
`roast/S04-phasers/end.t`: `keyword_literal`'s `BEGIN`/`INIT`/`CHECK`/`END`/…
phaser-prefix parsing tries `expression_no_sequence` on whatever follows the
keyword as the phaser's operand, so `END => "x"` first attempts "the `END`
phaser applied to the term `=> "x"`" before falling back to reading `END` as
a plain bareword (the correct parse, producing the Pair
`BareWord("END") => "x"`). A fatal error inside that speculative hypothesis
aborted the fallback outright. Keeping the error soft (recorded via the
existing `update_best_error` best-candidate mechanism, using the
`"X::Type: text"` message convention so `PError::typed_convention_message`
still promotes it once every alternative — including the correct one — has
failed) lets `primary()`'s normal alternation abandon a wrong hypothesis
exactly as it did before, while still surfacing the specific diagnosis when
`=>` genuinely has no valid term interpretation at all.

`src/parser/stmt/decl/my_decl_assign.rs`'s `malformed_initializer` already
anticipated this case in a doc comment (it declines to flatten an error into
`X::Syntax::Malformed` whenever `err.exception.is_some()`), so no change was
needed there — the new soft, exception-carrying error passes through
unchanged.

Verified against `t/malformed-syntax-classes.t` (test 4, no longer needing a
stub), `roast/S32-exceptions/misc2.t`'s `X::Syntax::InfixInTermPosition`
subtest, a manual regression sweep of ~440 roast files that use `=>`, and a
handful of `./tmp/` smoke scripts covering hash literals, named arguments,
list construction, and `Pair` construction to confirm ordinary `=>` usage is
unaffected.
