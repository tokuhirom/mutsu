# A sigilless `constant \NAME` inside a non-unit `module`/`package` block is invisible to a nested `sub` (bareword falls back to its own name as a string)

## Symptom

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

- raku: `Blob[uint8]:0x<FD>`
- mutsu: `EOR` (the bareword resolves to the **string** `"EOR"`, not the
  constant's value)

Found via the real-dist compat sweep on `RSV` (a small dist implementing the
[RSV binary format](https://github.com/Stenway/RSV-Specification)):
`lib/RSV.rakumod` declares `constant \EOV = blob8.new(255);` etc. directly
inside `module RSV { ... }` and references them from `sub to-rsv`/`sub
from-rsv`. All 16 of its own test-suite subtests fail this way (12 assertion
failures, 2 knock-on `is-deeply` failures from the corrupted blob, matching
`todo/tickets/dist-test-suite-failures-batch.md`'s "Un-triaged" `RSV` entry).

## Reduced repro and scope

- `constant \EOR = ...` + `sub` referencing it, both inside `module Name {
  }` or `package Name { }` (non-unit form) — **broken**, regardless of `my
  constant`/bare `constant`/`our constant`, and regardless of whether the sub
  is exported/imported or called qualified (`RSV::helper()`).
- The exact same shape inside `class Name { }`, referenced from a `sub` OR a
  `method` declared in the class body — **works correctly**.
- A **sigiled** `constant $EOR = ...` (scalar, not sigilless) inside a
  `module`/`package` block — **works correctly**. Only the sigilless
  (`constant \NAME`) form is affected.
- `unit module`/`unit class` form not yet tested; likely same as non-unit
  given it takes a different branch in `Stmt::Package` handling
  (`src/compiler/stmt.rs`, the `*is_unit` branch) but worth checking first.

## Root cause (traced 2026-08-06)

`--dump-ast` shows the AST is identical in the working and broken cases: the
constant reference compiles to a plain `Expr::BareWord("EOR")` in the sub/
method body either way. The divergence is entirely in how the **compiler**
(`src/compiler/`) resolves that `BareWord` at compile time, which depends on
how the enclosing declaration's body gets compiled:

- **`class` bodies never compile inline.** `Stmt::ClassDecl` lowers to a
  declaration plan (`Compiler::add_class_decl_plan`,
  `src/compiler/decl_plan.rs`) and the body is compiled later, at runtime,
  when `OpCode::RegisterDecl` executes (`src/runtime/registration_*.rs`).
  Empirically, whatever compiles the class body there keeps the class-level
  `constant \EOR`'s VarDecl and each method/sub's body compilation together
  in a way that resolves "EOR" to a real local read (`GetLocal`) — i.e. the
  constant and its readers share one compiler's `local_map` /
  `constant_vars_in_scope` directly, not through the enclosing-scope
  indirection below.

- **A non-unit `module`/`package` body compiles inline**, in the *same*
  `Compiler` instance as its surrounding scope
  (`Stmt::Package` handling in `src/compiler/stmt.rs`: `for s in body {
  self.compile_stmt(s); }`, confirmed by reading the code directly). So the
  `constant \EOR` VarDecl compiles via `self`, and (per `stmt.rs` around
  line 997) gets recorded in `self.constant_vars_in_scope` — but **not** in
  `self.sigilless_locals`. When the following `Stmt::SubDecl` for `helper`
  is compiled, `compile_sub_body` (`src/compiler/helpers_sub_body.rs`)
  creates a **fresh** `sub_compiler = Compiler::new()` and calls
  `self.inherit_enclosing_scopes(&mut sub_compiler)`
  (`src/compiler/mod.rs`), which propagates `self.sigilless_locals` into
  `sub_compiler.enclosing_sigilless` — but does **not** propagate
  `self.constant_vars_in_scope`. Since `EOR` was never in
  `self.sigilless_locals` to begin with, it ends up in neither
  `sub_compiler.local_map` nor `sub_compiler.enclosing_sigilless`, so
  `Expr::BareWord`'s compile arm (`src/compiler/expr.rs` around line 174)
  falls through every specific case to the generic `OpCode::GetBareWord`
  package-lookup fallback. At runtime, `exec_get_bare_word_op`
  (`src/vm/vm_var_get_ops.rs`) looks up the *unqualified* name `"EOR"` in
  `env()`, finds nothing (the constant's global was stored under the
  *qualified* key `RSV::EOR` via `qualify_variable_name` + `our_locals`, per
  `src/compiler/stmt.rs`'s `is_our` branch), and eventually falls all the way
  through every special case with nothing matching, landing on the ultimate
  string-fallback that returns the bareword's own name as a `Str`.

So there are two independent gaps, either of which alone might be enough to
fix this:

1. `Expr::BareWord`'s sigilless-constant resolution (`sigilless_locals` /
   `constant_vars_in_scope`) is keyed only off the CURRENT compiler's own
   maps; `inherit_enclosing_scopes` only forwards `sigilless_locals`
   (used for genuine `my \x`/`state \x`), not `constant_vars_in_scope`. A
   naive fix (also propagate `constant_vars_in_scope` into
   `enclosing_sigilless`, or add a parallel `enclosing_constant_names` set
   consulted the same way) needs to route through `OpCode::GetGlobal`,
   which reads by the **unqualified** name at runtime.
2. `OpCode::GetGlobal`'s runtime lookup (`exec_get_bare_word_op`'s sibling
   path, or `GetGlobal`'s own handler in `src/vm/vm_exec_dispatch.rs`)
   reads `env()` for the bare name only; the actual global store key is
   package-qualified (`RSV::EOR`, set via `our_locals` / `SetGlobalRaw`).
   Simply widening step 1 would still miss unless the runtime lookup also
   tries the current-package-qualified name as a fallback — which
   `exec_get_bare_word_op` already does for OTHER cases (types, enums,
   qualified subs) but does not currently attempt for a plain constant.

## Why this is a ticket, not a one-line fix

The fix touches the same `sigilless_locals` / `constant_vars_in_scope` /
`enclosing_sigilless` / `outer_constant_names` compiler-state cluster that
`src/compiler/mod.rs` and `src/compiler/stmt.rs` already carry extensive
scar-tissue comments about (ancestor-shadow detection, the
`shadows_outer_constant` X::Redeclaration logic, `outer_constant_names` vs
`constant_vars_in_scope` having deliberately different lifecycles). A change
here needs to avoid breaking:

- The existing shadow-detection semantics for `constant` redeclaration
  inside nested blocks/closures (`shadows_outer_constant` in
  `src/compiler/stmt.rs`).
- The ADR-0006 §2.2 compile-time constant-folding/inlining path
  (`note_constant_decl`/`constant_value` in `src/compiler/const_fold.rs`),
  which already has its own (separate, apparently working) mechanism for
  literal-valued constants — this bug is specifically about **non-literal**
  (runtime-computed, e.g. `blob8.new(253)`) sigilless constants, which can't
  be constant-folded and must go through the general BareWord/local-slot
  path instead.
- Whichever mechanism makes the `class`-body case work correctly today,
  since any propagation fix should ideally make `module`/`package` behave
  the same way rather than diverge further.

Needs a design pass on: should this be fixed by making `module`/`package`
non-unit bodies compile their nested subs through the SAME single-pass
mechanism the class-body registration path uses (closing the gap by
unifying, rather than patching the enclosing-scope propagation), or by
extending `inherit_enclosing_scopes` + the `GetGlobal`/`GetBareWord` runtime
fallback to handle a package-qualified sigilless constant read. The former
is likely more consistent with how `class` already avoids the bug; the
latter is more localized but touches the widest-blast-radius part of the
compiler (`Expr::BareWord`'s resolution chain, used by every bare identifier
read in the language).

## Repro commands

```
timeout 30 target/debug/mutsu -e '
module RSV {
    constant \EOR = blob8.new(253);
    our sub helper () is export {
        say EOR;
    }
}
import RSV;
helper();
'
# raku: Blob[uint8]:0x<FD>   mutsu: EOR
```

Full dist repro: `~/.cache/mutsu-dist-sweep/R_SV_RSV_*.tar.gz` (extract, then
`raku -I lib t/simple-cases.rakutest` vs. `mutsu -I lib
t/simple-cases.rakutest` — raku: 16/16 pass, mutsu: 2/16 pass with 14
`to-rsv`/`from-rsv` corrupted-blob failures).
