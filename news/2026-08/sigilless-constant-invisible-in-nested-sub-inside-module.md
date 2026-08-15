# A sigilless `constant \NAME` inside a `module`/`package` is now visible to a nested `sub`

```raku
module RSV {
    constant \EOR = blob8.new(253);
    sub helper () is export {
        say EOR;
    }
}
import RSV;
helper();
```

was printing the bareword's own name as a string (`EOR`) instead of the
constant's value (`Blob[uint8]:0x<FD>`) — but only for a **non-unit**
`module`/`package` body, and only for a constant whose initializer is not
compile-time-foldable (a method call like `blob8.new(253)`, not a plain
literal — a literal-valued constant already resolves via the existing
ADR-0006 §2.2 constant-folding path). The same shape inside a `class` body,
and a **sigiled** `constant $NAME` (referenced with its sigil) inside a
module, already worked correctly. Found via the real-dist compat sweep on
`RSV` (all 16 of its own test-suite subtests hit this).

## Root cause

The AST is identical in the working and broken cases — a plain
`Expr::BareWord("EOR")` — so the divergence is entirely in *compile-time*
bareword resolution. A non-unit `module`/`package` body compiles inline in
the same `Compiler` as its surrounding scope; the `constant \EOR` VarDecl
gets recorded in `self.constant_vars_in_scope`, but when the following `sub`
is compiled, `compile_sub_body` builds a **fresh** `Compiler` and
`inherit_enclosing_scopes` only propagates `sigilless_locals`/`local_map`,
never `constant_vars_in_scope`. So `Expr::BareWord`'s compile arm falls
through to the generic `OpCode::GetBareWord` runtime fallback, which looks up
the *unqualified* name in `env()` — but the constant's value is stored under
the *qualified* key (`RSV::EOR`, via `qualify_variable_name` + `our_locals`)
— and, finding nothing, degrades to returning the bareword's own name as a
`Str`.

(The `class`-body case turned out to work for an unrelated, somewhat
incidental reason: a class method body also compiles through a fresh
`Compiler` and also emits `GetBareWord` — but `OpCode::PackageScope`, which a
non-unit `module`/`package` body runs under, snapshots and restores/filters
`env` on exit, discarding a bare-name env residue that a class body's
registration path never rolls back. Not a mechanism worth relying on or
replicating.)

## Fix

`exec_get_bare_word_op` (`src/vm/vm_var_get_ops.rs`) already had every other
kind of qualified-name fallback (types, enums, qualified subs) except a
package-qualified fallback for a *plain value*. Rather than plumb a new
compile-time propagation path through the `sigilless_locals` /
`constant_vars_in_scope` / `enclosing_sigilless` / `outer_constant_names`
cluster (which the original investigation flagged as carrying "extensive
scar-tissue" from prior shadow-detection and const-folding work), the fix
reuses `package_chain_var_fallback` — the same package-chain walk
`OpCode::GetGlobal` already applies for a qualified *sigiled* variable read —
as a new fallback branch for a bareword. It walks the current package chain
trying `our_vars`/`env` for `{pkg}::{name}`, so it needs no compiler changes
at all and cannot shadow anything: every more specific resolution (types,
enums, functions, an unqualified `our`-var) already had first refusal, and it
is a no-op outside package context (`current_package()` empty/`GLOBAL`).

One wrinkle the same fix needed twice: a single-character bareword (`A`,
not `EOR`) hits an entirely separate branch of `exec_get_bare_word_op` with
its own complete fallback chain (vulgar fractions, single-digit numerics,
an *unqualified* `our`-var check) that also terminates in the string
fallback — so the same `package_chain_var_fallback` call had to be added
there too, caught by the new regression test exercising both a multi-char
and (implicitly, via the shared code path) single-char constant name.

## Verification

The ticket's exact repro, plus `module`/`package`/`my constant`/`our
constant` variants, plus the already-working `class`-body and sigiled-`$`
cases (regression guards) — all now match `raku`. New
`t/sigilless-constant-nested-sub-inside-module.t`; full local `t/` suite
(29,583 tests) unaffected. The real `RSV` dist's `t/simple-cases.rakutest`
went from 2/16 to 11/16 passing (all remaining 5 are an unrelated
itemization-sigil mismatch in `from-rsv`'s return value, recorded separately
in `todo/tickets/rsv-from-rsv-result-extra-itemization-sigil.md`).
