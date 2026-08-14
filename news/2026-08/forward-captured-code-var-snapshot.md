# Forward-referenced `&`-sigil lexicals resolve live instead of freezing a stale Nil

A closure that reads a `&`-sigil (code) lexical **bare**, as a value rather
than a call (e.g. `.map(&decode)`, or `say &f`), before the enclosing `my
&name = ...` declaration had even *compiled* — let alone run — used to freeze
that variable's pre-declaration local slot (still `Nil` at that point) into
the closure's own captured environment forever. Real Raku instead resolves
the reference live: as long as the assignment has run by the time the
closure is *called* (not created), the forward reference just works. This is
the `&`-sigil twin of the value-typed-scalar snapshot bug fixed earlier for
captured `my int` / `Int:D is rw` locals (`vm_register_ops.rs`'s
cell-boxing loop).

## Root cause

`CompiledCode::compute_free_vars` (`src/opcode.rs`) has two separate code
paths that decide whether a `&name` reference inside a closure body counts
as a *captured free variable* (baked into the closure's env snapshot at
creation time): one for a bare call `f(...)`, one for a bare value read
(`&f`, `&f.()`). The call-form path was already correctly gated on
`outer_code_var_names` — a table, threaded down at compile time in source
order, of `&`-sigiled names an enclosing scope has *actually declared by this
point* — so a forward reference like CBOR::Simple's

```raku
my &decode-array = { ... .map(&decode) ... };   # &decode not declared yet
...
my &decode = { ... };                            # declared here
```

correctly left `&decode` OUT of `decode-array`'s captured free variables,
deferring resolution to call time (by which point `&decode` has been
assigned, and mutsu's live env-chain lookup finds it). But the bare-value-read
path had no such gate: it captured `&decode` unconditionally, regardless of
whether it had been declared yet, freezing its still-`Nil` local slot into
`decode-array`'s closure. Every later `.map(&decode)` inside `decode-array`
then read that frozen `Nil` — "Cannot map a Nil to a Range".

## Fix

Made the bare-value-read gate symmetric with the already-correct call-form
gate: `&name` is only added to `free_var_syms` when `outer_code_var_names`
proves it is a real lexical binding already visible at this point in source
order (own params are always visible, since they're declared before their
sub's body starts compiling — this is what keeps `roast/integration/man-or-
boy.t`'s `&x1`-parameter captures working). A genuine forward reference is
left out of the eager capture and instead resolves through the same live
env-chain lookup the call-form path already relied on.

Verified with `rust-gdb` (not eprintln rebuilds, per the project's debugging
guidelines): stepped through `resolve_code_var` and `capture_closure_env` to
confirm the pre-fix stale-Nil bake site, and confirmed post-fix that
`free_var_syms` correctly excludes the forward reference while a genuinely
already-declared `&`-sigil parameter (the man-or-boy shape) is unaffected.

Added `t/forward-captured-code-var.t`, pinning: the ticket's own minimal
call-form repro, a bare-value-read forward reference, the same forward
reference passed as a bare `.map()` argument (CBOR::Simple's exact shape),
a two-closure mutual-recursion shape matching `cbor-decode`'s
`decode-array`/`decode`/`decode-pair` dispatch, and a non-forward (ordinary)
bare `&f` read to guard against regressing the common case.

## Impact

Manually verified with `mutsu -I modules/CBOR-Simple/lib`: `cbor-encode` →
`cbor-decode` round-trips now work for int/string/array/nested-map values,
including the mutually-recursive `decode-array`/`decode-map`/`decode`
dispatch that this bug previously blocked entirely
("`Cannot map a Nil to a Range, it's not callable`"). The *full* round-trip
(every value shape `Simple.rakumod` supports) is still blocked by a
separate, unrelated parser bug (an `elsif` chain misparsed as a bareword
call when it immediately follows a `my constant` declaration) — tracked in
`todo/tickets/cbor-simple-typed-array-and-diagnostic-format-gaps.md`, not
fixed here since its root cause (compiler statement-boundary handling around
`my constant`) is unrelated to closure capture.

While investigating, also found and filed a separate pre-existing bug
(`todo/tickets/sibling-block-code-var-name-leak.md`): two **sibling**
top-level blocks that each declare a `&`-sigil lexical of the same name
(e.g. two blocks each with their own `my &f`/`my &g`) corrupt the second
block's name resolution — reproducible with a plain bare call `f()`, so it
predates and is independent of this fix.
