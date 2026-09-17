# An inline `constant $ = EXPR` no longer gets boxed into a Scalar container

An anonymous `constant $ = EXPR` used in expression position — `(constant $ =
blob32.new: ...)`, the idiom the vendored `Digest::SHA` module uses to cache
its SHA-1 initial hash constants without paying an allocation on every call —
was silently getting itemized into a Scalar container, exactly as an ordinary
`my $x = EXPR` would. A Raku `constant` has no container at all: it binds its
name directly to the raw value, like `:=`. Passing such a constant straight
into a typed positional parameter, or into any other list-context consumer
(a `Z` zip, a slurpy `|`), therefore saw ONE boxed item instead of the
constant's real elements.

Root cause: `compile_expr_do_stmt` (`src/compiler/expr_block.rs`), which
compiles a `Stmt::VarDecl` appearing in expression position, never checked the
declaration's `__constant` trait. The anonymous form compiles to a `VarDecl`
named the synthetic `__ANON_STATE__` with `is_our: true`, and that arm:

1. published the value via a plain `SetGlobal` instead of `SetGlobalRaw` — the
   opcode the *statement*-position `constant` path already used, whose
   general-store handler skips Scalar-container itemization only under
   `raw_mode`;
2. unconditionally emitted `WrapScalar` afterwards, a step meant only for the
   different, genuinely-itemizing `my $ = EXPR` idiom that happens to share the
   same synthetic `__ANON_STATE__` name.

Both are now conditioned on the declaration's `__constant` trait, and the same
named-constant case (`(constant FOO = EXPR)`) gets a local slot so the
existing `MarkConstantContext`-aware `SetLocal` store applies too.

Found while getting `Digest::PSHA1` to run its own test suite under mutsu:
`Digest::SHA`'s `sha1-block` reduces over exactly such an anonymous constant
accumulator, and the truncated container silently produced a wrong SHA-1
digest instead of erroring. Pinned by
`t/vm/expr-position-constant-no-itemize.t`.

A second, unrelated bug remains in the same distribution's test — a typed
block-signature parameter's constraint leaking by name into an unrelated
same-named variable in a different call frame — filed as
[#8614](https://github.com/tokuhirom/mutsu/issues/8614).
