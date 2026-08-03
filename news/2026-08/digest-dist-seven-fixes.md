# Seven general fixes found by running the `Digest` distribution

Running grondilu's `Digest` distribution (`Digest::MD5`, `Digest::SHA1`,
`Digest::SHA2`, `Digest::SHA3`, `Digest::RIPEMD`, `HMAC` — the dependency of
`Digest::HMAC`) surfaced seven independent interpreter bugs. The distribution is
dense, idiomatic Raku over native buffers — `blob32`/`buf64`, `Z+` over Blobs,
`reduce` with a routine reference, placeholder-block dispatch tables, an
infinite `constant @` — so almost every construct it uses hit a different gap.
None of the fixes is Digest-specific; each is a general correctness fix pinned by
its own `t/` file.

`Digest::SHA1`, `Digest::SHA2`'s `sha224`/`sha256` now produce correct digests
under mutsu.

## 1. A colon-method argument list did not absorb the list-infix operators

`blob32.new: $H Z+ $M` parsed as `(blob32.new: $H) Z+ $M`. Raku's list-infix
operators (`Z`, `X`, meta-ops, `minmax`) are LOOSER than the comma separating a
list-prefix's arguments, so the whole comma level is one operand. The bare listop
path (`say 100, 200 Z+ 42, 23`) already did this via `extend_listop_arg_list_infix`
+ `lift_list_infix_in_arg_list`; none of the four colon-method argument loops
(postfix `.m:`, private `!m:`, hyper `».m:`, topic `.m:`, and `.=m:`) did. They
now share `listop_arg_expr_list_infix`. Silent wrong values, not an error:
`Digest::SHA1`'s `sha1-block` returned a `Seq` and the digest came out as the
unchanged initial constants. Pin: `t/colon-method-arg-list-infix.t`.

## 2. `@$x` was conflated with `@x`

The parser lowered `@$x` to `Expr::ArrayVar("x")` — indistinguishable from the
separate array variable `@x`. With both `my $b` and `my @b` in scope, `@$b` read
`@b`; and inside a routine `@$_` read the implicit slurpy `@_` (so it returned the
call's arity, not the topic). `Digest::SHA1`'s `map { blob32.new: @$_ }, …` therefore
collapsed each 16-word chunk to a single element. `@$x` now builds the same
`.list`-on-`Grouped` node that the spelled-out `@($x)` has always built — which
also makes `@$x[0] = …`, `@$x.push(…)` and `push @$x, …` write through the scalar's
container (they used to silently mutate nothing). The `%$h` counterpart was
already correct. Pin: `t/array-sigil-scalar-deref.t`.

## 3. An `@`-sigil read of a Buf/Blob did not yield its elements

A Buf/Blob is Positional, so `@$blob` is its element list, which flattens and
slips element-wise. `GetArrayVar` returned the Blob unchanged, so
`flat @$msg, 0x80` kept the whole Blob as one element and SHA-1's padding produced
13 words instead of 14. Same file pins it.

## 4. Placeholders in a nested block were attributed to the enclosing signature

The "placeholder cannot override existing signature" check scanned the block body
with the DEEP `collect_placeholders`, so a nested block's own `$^a` was reported
as an override of the outer block's explicit signature. It now uses
`collect_unattached_placeholders` — the same collector the `sub` declaration check
uses, which stops at every nested `{}` — with declared parameters filtered out so
an explicitly-declared `@_` (`-> @_ { … @_ }`) stays legal. This is what made
`Digest::MD5` and `Digest::RIPEMD` fail to compile at all. Pin:
`t/placeholder-nested-block-scope.t`.

## 5. A `-->` return type leaked into closures created inside the routine

Closure creation copies the enclosing env, which carries `__mutsu_return_type`.
A block created inside `sub f(--> blob32)` therefore had ITS OWN return checked
against `blob32`. It only surfaced when the routine was invoked through a
Callable value (`reduce &sha1-block, …`, `my &f = &f2`), because the by-name
dispatch path reads the return spec from the routine's own metadata. Closure
creation now clears any inherited `__mutsu_return_type` before installing its own.
Pin: `t/return-type-not-inherited.t`.

## 6. Native integer arrays did not wrap on push, and did not accept a Blob

`my uint32 @W; @W.push(6535351809)` stored the un-truncated value where an
assignment stores `2240384513`, so SHA-1's message schedule — whose rotate relies
on uint32 truncation — computed the wrong digest. Every push/append/unshift/prepend
onto a native integer array now wraps to the element width. Separately,
`my uint32 @W = $M` where `$M` is a `blob32` now spreads element-wise (rakudo's
`array[uint32].STORE` reads the buffer directly); a boxed `my Int @b = $blob` still
sees the Blob as one element, as in rakudo. Pin: `t/native-int-array-store.t`.

## 7. `constant @x` could not hold a lazy list

`constant @primes = grep *.is-prime, 2 .. *` (`Digest::SHA2`) wrapped the infinite
lazy list as a SINGLE element, so `@primes[^8]` read `((...) Nil Nil …)`. A lazy
list now stays lazy behind a `constant @` exactly as behind a `my @`. Note this
required the same fix in all THREE copies of the "constant @ coercion" logic
(`OpCode::CoerceToList`, `SetLocal`'s constant branch, and `SetGlobalRaw`) — worth
collapsing into one helper next time one of them is touched. Pin:
`t/constant-array-lazy.t`.

## Still open

`Digest::MD5`, `Digest::RIPEMD`, `sha512`/`sha384` and `HMAC` do not run yet;
the remaining blockers are recorded in `todo/tickets/digest-dist-blockers.md`.
