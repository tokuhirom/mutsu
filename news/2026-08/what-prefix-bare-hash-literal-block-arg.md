# `WHAT {...}` no longer misparses as two statements

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Hash.rakudoc:65`).

`WHAT`/`HOW`/`VAR` are prefix pseudo-routines that take a term. Every shape of
argument worked except a bare `{...}` immediately following the prefix — with
no enclosing parens and no intermediate variable:

```raku
say WHAT(3);                  # (Int)  -- parens: OK
say WHAT 3;                   # (Int)  -- bare non-block term: OK
my $h = {3=>4}; say WHAT $h;  # (Hash) -- via variable: OK
say WHAT {3 => 4};            # should be (Hash), mutsu printed "WHAT"  -- BROKEN
```

mutsu parsed `WHAT` as a bare identifier (which stringifies to the literal
`"WHAT"`) and `{3 => 4}` as a separate block statement run in sink context —
hence the accompanying "Useless use of ... in sink context" warning.

## Cause

The generic no-paren "identifier followed by a term is a listop-style call"
fallback in `identifier_or_call()` (`src/parser/primary/ident/identifier_call.rs`)
gates on the next character actually starting a term: `$`/`@`/`%`/digits/quotes/etc.
A bare `{` was deliberately excluded from that gate, because for an arbitrary
undeclared bareword a following block is usually the *enclosing* construct's
body, not the identifier's argument (e.g. `$x ~~ Sub-Test { ... }` must leave
the block to the enclosing `if`/`when`). That exclusion is correct for
arbitrary identifiers, but wrong for the fixed set of pseudo-routines that
unambiguously take a single term: a bare `{...}` right after `WHAT`/`HOW`/`VAR`
can never belong to anything else.

## Fix

Added `is_prefix_pseudo_op()` (`src/parser/primary/ident/predicates.rs`) —
`WHAT`/`HOW`/`WHO`/`WHICH`/`WHERE`/`DEFINITE`/`VAR` — and let the term-start
gate accept `{` when the head name is one of these. The rest of the machinery
was already correct: once the parser attempts to parse the argument, it
reaches `block_or_hash_expr()`, the same hash-vs-block disambiguation `say
{...}` already relies on, so `WHAT {3 => 4}` now correctly yields `(Hash)`
while `WHAT { 1 + 1 }` yields `(Block)`, matching rakudo.

(`WHO`/`WHICH`/`WHERE`/`DEFINITE` parse correctly now too, though they are not
yet wired up as callable prefix functions at runtime — a separate, pre-existing
gap unrelated to this parsing fix.)

Verified against all four examples in `raku-doc/doc/Type/Hash.rakudoc:65-68`.
Pinned by new cases in `t/what-subroutine.t`.
