# A method's expression-position `my` writes through a captured caller lexical's cell

**Status (re-verified 2026-08-20 against `main` @ `e57c04680`): still real, but far
smaller than first recorded — and READY FOR DIRECT IMPLEMENTATION.** The fix was
prototyped end-to-end during this re-verification: it is a two-site change with a
discriminator that already exists at compile time, `make test` is clean, and both
roast pins that guard the opposite direction stay green. It needs no design and no
ADR. This file moved from `todo/deep/` to `todo/tickets/` for that reason.

## What is already fixed (do not re-chase it)

The originally recorded repro no longer reproduces:

```raku
my $g = "outer";
sub f(*%args) { if (my $g = %args<g> :delete) { return 1 }; 42 }
EVAL q[f()];
say $g.raku;   # both raku and mutsu: "outer"
```

Named subs, anonymous subs, multi subs and `EVAL`-carrier calls all keep the
caller's lexical intact now, whether the caller's `my` is declared before or after
the callee, and whether or not a sibling escaping closure has forced the caller's
lexical into a shared `ContainerRef` cell. ADR-0024's unit-lexical cells plus the
free-var-write drain from the original expr-decl-leak PR cover those shapes.

## What still leaks: every *method* dispatch flavour

The surviving hole is method bodies. Minimal repro:

```raku
my $a = "A";
my $keep = sub { $a = $a };            # escaping closure -> $a becomes a shared cell
class C { method m() { if (my $a = 0) { }; 42 } }
C.m();
say $a.raku;                            # raku: "A"   mutsu: 0
```

`$keep` never even has to be called. A probe matrix (`tmp/scope.p6` during the
investigation) showed the leak on **class method, role method, submethod, instance
method, multi method, private method, method invoked from a `.map` block, and
method invoked through an intermediate sub** — 8 of 8 method shapes — while multi
sub and the `@`/`%` container shapes are clean. Every leaking case silently
overwrites the caller's variable with the declaration's value (`0` / `Any`).

## Root cause (confirmed with `rust-gdb`, not guessed)

Breaking on the two write-through sites and reading `self.vardecl_context` at each
hit shows exactly the mechanism:

1. `call_compiled_method_fast` (`src/vm/vm_method_dispatch.rs:1700`) runs the
   method body on an env that already holds the caller's `ContainerRef` cell under
   the bare name `a`.
2. The method's expression-position `my $a` has no local slot (the compiler only
   allocates one when the name shadows a *known enclosing local*; a free variable
   gets none), so it compiles to `MarkVarDeclContext; SetGlobal`.
3. `SetGlobal`'s generic "Write through ContainerRef" branch
   (`src/vm/vm_exec_dispatch.rs:1489-1501`) finds that cell and stores the
   declaration's value into it. `vardecl_context` is `true` at that hit and `false`
   at the legitimate write from `$keep` — so the flag *does* discriminate here.
4. Gating **only** that branch changes nothing: the store then falls through to
   `set_env_with_main_alias` (`vm_exec_dispatch.rs:1672`) and the *same*
   write-through repeats inside `set_env_with_main_alias_sym`
   (`src/vm/vm_env_helpers.rs:974-980`), which is reached after the handler has
   already cleared the flag at line 1585. Both sites must be handled together.

The reason methods are the only survivors is structural: a class/role method's
`CompiledCode` is registered separately from the enclosing compile unit, so it
never appears in the enclosing frame's `closure_compiled_codes`. Every
`expr_declared_syms`-based filter that protects subs — the capture filter, the
free-var-write drain, the return-merge exclusions — is keyed off that list and
therefore simply never runs for a method.

## The fix, and why the earlier "needs frame-ownership analysis" premise was wrong

The previous version of this file assumed the correct rule ("a declaration shadows
an INHERITED cell but a same-frame redecl keeps its own cell") required
distinguishing frame-own from inherited env entries at runtime, which flat cloned
envs cannot do. **That is not needed.** The discriminator already exists at compile
time, per frame, in the frame's own bytecode: `CompiledCode::expr_declared_syms`.

`src/compiler/expr_block.rs:154-159` inserts a name into `expr_declared_syms`
exactly when the expression declaration is a genuinely fresh binding, and
deliberately *skips* it when the declaration is `promoted` — the synthesized
`WhateverCode` case, where the declaration lexically belongs to the surrounding
source block and therefore MUST write through the enclosing frame's cell
(`roast/S02-types/whatever.t` #45, mirrored by `t/expression-position-my-scope.t`
#8). The two classes are complementary by construction, so consulting
`expr_declared_syms` needs no new analysis, no new opcode, and no runtime
ownership tracking.

Prototype (validated, then reverted — implement it cleanly, do not copy verbatim):

- In the `SetGlobal` arm of `src/vm/vm_exec_dispatch.rs`, before the write-through
  branch, compute
  `let fresh_binding_decl = self.vardecl_context && code.expr_declared_syms.contains(&Symbol::intern(&name));`
  (`code` is already in scope there — line 1507 already passes it to
  `escaping_our_write_cell`). Skip the `ContainerRef` write-through at 1489 when it
  is set.
- Give `set_env_with_main_alias_sym` the same signal so its own write-through at
  `vm_env_helpers.rs:974` is skipped for a fresh binding. **Do not thread it as the
  ambient `vardecl_context` flag** (the prototype re-armed the flag around the call
  purely to prove the semantics; the flag is consumed en route and 95 call sites
  make a new parameter on `set_env_with_main_alias` invasive). Add a dedicated
  entry point — e.g. `set_env_with_main_alias_fresh_binding(&name, val)` — used only
  by this declaration path, with both variants delegating to one inner helper that
  takes an explicit `fresh_binding: bool`.

## Validation already performed on the prototype

- All 8 leaking method shapes and all 5 sub/method shapes in the probe matrix match
  `raku` exactly.
- `roast/S02-types/whatever.t` (131 tests) — PASS, including #45.
- `roast/S02-types/pair.t` (182 tests) — PASS, including #181.
- `t/expression-position-my-scope.t` — 8/8, including #7/#8 (the promoted
  `WhateverCode` pair).
- Full `make test`: 3270 files / 30365 tests, **zero regressions**. The only
  failure, `t/compunit-can-install.t` #4, fails identically on unpatched `main` in
  this container (it runs as root, so the "non-writable root" premise does not
  hold) and is unrelated.
- Edge probes that were *improved*, not just preserved: an expression-position
  `our $ov` inside a method stopped clobbering the file-scope `our $ov`, matching
  `raku` (unpatched mutsu wrote `"inner"`, raku and the prototype both give
  `"pkg"`). Genuine (non-declaration) method writes to a captured caller lexical
  still write through, as they must.

Note that the naive alternative — gating both sites on the ambient
`vardecl_context` alone, without the `expr_declared_syms` check — regresses exactly
one thing: `whatever.t` #45 / `expression-position-my-scope.t` #8, the promoted
`WhateverCode` declaration. That is the single reason the compile-time set has to
be consulted rather than the flag alone.

## Implementation checklist

- The two VM sites above.
- Extend `t/expr-decl-lexical-no-leak.t` with the method-plus-shared-cell shape
  (the `$keep`/`class C` repro), and refresh its comment at lines 42-45: the
  "declared before the sub" caveat it records is obsolete for subs, and its path
  reference now points at `todo/tickets/`.
- Refresh the stale pointers in `docs/batteries/csv.md`,
  `news/2026-08/expr-decl-lexical-leak.md` and
  `docs/adr/0032-wrapvarref-container-capture-across-closure-boundaries.md` §
  (all still say `todo/deep/...`).
- Retire the "the store still writes through to us, which is the pre-existing scope
  leak roast S02-types/whatever.t #45 pins" sentence in `src/opcode.rs` (~line
  5806): once the store stops writing through, that comment describes behaviour
  that no longer exists, and #45 is pinned by the `promoted` exclusion instead.
- On resolution, `git mv` this file to `news/2026-08/`.
