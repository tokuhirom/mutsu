# Sibling top-level blocks reusing a `&`-sigil lexical name corrupt forward-reference resolution

Discovered while fixing `todo/tickets/forward-captured-code-var-snapshot.md` (see
`news/2026-08/forward-captured-code-var-snapshot.md`). This is a **separate,
pre-existing** bug, unrelated to that fix — it reproduces identically with a
plain bare call (`f()`), a form that was never affected by the closure-capture
fix.

## Repro

```raku
{
    my &g = -> { f() };
    my &f = -> { 100 };
    say g();
}

{
    my &g = -> { f() };
    my &f = -> { 200 };
    say g();
}
```

Expected (raku): `100` then `200`. mutsu: `100` then `Unknown function: f`.

The same shape also reproduces with two top-level `sub outer { ... }`
declarations of the same name, each independently declaring `my &f`/`my &g`
inside.

## Root cause (partial diagnosis)

`Compiler::inherit_outer_code_var_names` (`src/compiler/helpers_sub_body.rs:141`)
threads `self.local_map.keys()` (filtered to `&`-prefixed names) down to a
child closure's `CompiledCode::outer_code_var_names`, so
`CompiledCode::compute_free_vars` can recognize a bare call `f(...)` as a read
of an enclosing `&f` binding (see `opcode.rs` around `op_callee_name_const_idx`
and the parallel `op_code_var_read_const_idx` handling for a bare `&f` value
read). This mechanism is meant to reflect "has `&f` been declared **at this
point in source order** within the block currently compiling" — but it appears
to leak across **sibling** block scopes: after the first `{ ... my &f = ...
}` block finishes compiling, the compiler's `local_map` entry for `&f`
apparently isn't cleared/reset before compiling the second, unrelated `{
... }` block that happens to also declare a lexical named `&f`. The second
block's own `&f` declaration should shadow/reset that name, but the observed
symptom (`Unknown function: f`, i.e. resolution fails entirely rather than
finding either sibling's binding) suggests the leaked/stale bookkeeping
actively interferes rather than merely mis-timing.

Not yet root-caused to an exact line — the diagnosis above is the minimum
established via black-box testing (comparing with/against reusing the same
name), not via `rust-gdb` breakpoints on the actual leak site. The next step
is a breakpoint on `Compiler::inherit_outer_code_var_names` (or wherever
`local_map` entries are removed when a block scope closes) to see whether
`&f`'s entry from the first block is actually still present when the second
block's closures compile.

## Impact

Low in practice — the reused-name-across-sibling-blocks shape is rare (most
code either uses distinct names or nests declarations inside a shared
enclosing scope, which does not hit this). Not currently blocking any known
roast test or bundled battery. Filed to avoid losing the repro; not fixed here
because it is unrelated in root cause to the forward-capture fix that
surfaced it, and fixing it would have broadened that PR's blast radius into
the compiler's block-scope bookkeeping for `local_map`, a different subsystem.
