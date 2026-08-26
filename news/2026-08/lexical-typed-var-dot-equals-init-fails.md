# `my Type:D $x .= new` was already fixed — closed with a regression pin

`todo/tickets/lexical-typed-var-dot-equals-init-fails.md` reported that a lexical
declaration whose type carries a definedness smiley failed its `.=` initializer:

```
$ mutsu -e 'my Lock:D $reg-lock .= new; say $reg-lock'
X::Method::NotFound: Unknown method value dispatch (fallback disabled): new on Lock:D
```

because `handle_method_call_assign` built the call target from the type-constraint
string verbatim, so the invocant became the literal bareword `Lock:D`.

## Outcome: stale

Re-verified on `main` at `e3886064d` against `raku` v2026.06 — it no longer
reproduces. `src/parser/stmt/decl/my_decl_assign.rs` now strips the smiley before
building the bareword target:

```rust
Some(c) => Expr::BareWord(
    crate::parser::stmt::decl::strip_type_smiley_suffix(c).to_string(),
),
```

with a comment recording exactly the reasoning the ticket asked for (the smiley
constrains the *variable*, not the invocant of the `.=` call; the `@`/`%` arms
above deliberately keep it, because there it is the *element* type and
`Array[Int:D]` is the real container type).

mutsu and raku now agree on the whole matrix the ticket listed: `Lock:D`,
`Int:D`, `Str:D`, a user class with `:D`, the no-smiley form, and `:U`
(which correctly fails its type check in both — a `:U` variable cannot hold the
object `.new` just built). The `our`-scoped case the ticket also listed turns
out not to be legal Raku at all: rakudo rejects `our Lock:D $x` at compile time
with "Cannot put a type constraint on an 'our'-scoped variable".

Since nothing pinned this, the matrix is now asserted in
`t/lexical-decl-and-autoviv.t` so it cannot silently regress.

One residual, cosmetic divergence was noted and left alone: on the failing `:U`
case mutsu's message says `expected Lock:U but got Any` where rakudo says
`expected Lock:U but got Lock (Lock.new)` — the exception type and the fact of
dying match; only the "got" rendering differs.
