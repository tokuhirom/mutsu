# A leading `{ ... }` before a custom infix operator is never parsed as its LHS operand

## Root cause

Raku's statement grammar has a well-known ambiguity: a `{` at the start of a
statement can begin either a bare block (a standalone statement, its own
lexical scope) or a term (a hash literal, or — the case here — the left
operand of an expression that continues past the closing `}`). Rakudo
resolves this by looking ahead past the matching `}`: if the next token
cannot itself start a new statement (e.g. it's an infix operator token, known
because the operator was already declared earlier in the file), the `{...}`
is a term, not a complete statement, and parsing continues into the infix
expression.

mutsu's statement parser does not perform this lookahead for a *custom*
(user-declared) infix operator. `{ ... }` at statement position is committed
to as a complete bare-block statement unconditionally, so a following custom
operator token starts a brand-new (and bogus) statement:

```raku
sub infix:<zork>(&closure, Int $num) is export {
    say "called with num=$num";
}
{ say "hi"; } zork 25;
```

mutsu: `{ say "hi"; }` runs as a bare block (prints "hi"), then a NEW
statement `zork 25;` is parsed — since `zork` isn't a declared *function*
(only `infix:<zork>` is), this fails with `Undeclared routine: zork used`.
raku: parses the whole thing as one `infix:<zork>` call and prints `called
with num=25` (the block is never independently executed as a statement — it's
consumed as the `&closure` argument).

Confirmed this is not about bareword-vs-symbolic operator spelling — the same
failure reproduces with a fresh symbolic operator too (`{ say "hi"; } ⚡ 25;`
→ `Confused. expected statement`), so the bug is squarely "a leading `{...}`
never looks ahead for ANY infix continuation," not something specific to word
operators.

## Affected files

The exact statement/term disambiguation site was not pinned down in this
investigation session (out of scope for the fix that motivated finding this —
see below); it lives somewhere in the top-level statement dispatch under
`src/parser/stmt/` (`mod.rs` and friends), specifically wherever a leading
`TokenKind::LeftBrace` at statement position currently commits unconditionally
to a bare-block-statement parse. `src/parser/stmt/control/labeled_loop.rs:118`
and `src/parser/stmt/modifier.rs:207` both have comments acknowledging
"bare block" as a distinct parse target, and are candidate starting points for
tracing the decision point, but neither was confirmed as the actual site.

## Why it is large

- This is a genuine grammar-ambiguity resolution, not a narrow bug: the fix
  needs lookahead past the block's matching `}` to the next token, checked
  against the SAME "known declared infix operators" table the expression
  parser already consults elsewhere (custom operators are typically
  registered as they're declared, mid-file, which is itself a parser-state
  dependency worth confirming holds here too).
- The fix has to avoid regressing the (far more common) legitimate bare-block
  statement case — `{ ... }` followed by literally anything else (a new
  statement, EOF, a closing brace of an enclosing block) must keep parsing as
  a bare block exactly as today.
- It plausibly interacts with existing special-cased leading-block handling
  (labelled bare blocks / `do` blocks per `labeled_loop.rs`, statement
  modifiers per `modifier.rs`), so the change needs to thread through those
  existing paths rather than add a parallel one.
- Precedence also matters once the lookahead succeeds: the resulting
  `Expr::InfixFunc`-style parse has to respect the declared operator's own
  precedence/associativity relative to whatever follows on the same line.

## Repro

```raku
sub infix:<zork>(&closure, Int $num) is export {
    say "called with num=$num";
}
{ say "hi"; } zork 25;
```

Expected (raku): prints `called with num=25` only.
Actual (mutsu): prints `hi`, then `===SORRY!=== ... Undeclared routine: zork
used`.

Found via the `PSpec` distribution's own test suite
(`todo/tickets/dist-test-suite-failures-batch.md`): its `xxx` helper is
`sub infix:<xxx>(&closure, Int $num) { $num times &closure }`, called as
`{ $value--; } xxx 25;` — exactly this shape. `PSpec`'s sibling bug (a closure
argument to a custom infix operator not writing its outer-lexical mutation
back to the caller — the `times` helper, `20 times { $value++ }`) was a
separate, already-fixed issue (`news/2026-08/user-infix-closure-arg-writeback.md`
if present, or see the `t/user-infix-closure-arg-writeback.t` regression
test); this ticket is only the remaining `{...} OP arg` parse gap.
