# A method body's expression-position `my` no longer clobbers a shared-cell caller lexical

The sibling shape left open by the earlier expression-position-`my` leak fix
(`news/2026-08/expr-decl-lexical-leak.md`) is now closed too. Minimal repro:

```raku
my $a = "A";
my $keep = sub { $a = $a };            # forces $a into a shared ContainerRef cell
class C { method m() { if (my $a = 0) { }; 42 } }
C.m();
say $a.raku;                            # was: 0 (bug) -- now: "A"
```

`$keep` never even has to be called. Merely capturing `$a` in an escaping
closure forces it into a shared `ContainerRef` cell (so the closure sees
later mutations); once that cell exists, ANY method whose body declares an
unrelated `my $a` in expression position (an `if`/`while` condition) found
that cell by bare env key and wrote the declaration's value straight through
it, silently overwriting the caller's variable. A probe matrix found the leak
on all 8 method-dispatch shapes tried: class method, role method, submethod,
instance method, multi method, private method, method invoked from a `.map`
block, and method invoked through an intermediate sub — while the equivalent
plain-sub shapes were already clean from the earlier fix.

## Root cause

A class/role method's `CompiledCode` is registered separately from the
enclosing compile unit's frame, so it never appears in that frame's
`closure_compiled_codes`. Every `expr_declared_syms`-based protection that
shields a sub's expression-position declaration from this leak (the capture
filter, the free-var-write drain, the return-merge exclusions) is keyed off
that list — and so simply never ran for a method body. The write-through
itself happens at the `SetGlobal` opcode: an expression-position `my $a` with
no local slot (the compiler only allocates one when the name shadows a
*known enclosing local*; a free variable gets none) compiles to
`MarkVarDeclContext; SetGlobal`, and `SetGlobal`'s generic "write through an
existing `ContainerRef` in env" branch finds the caller's cell under the bare
name and stores the declaration's value into it.

## The fix

The discriminator needed already existed at compile time, per frame, in the
frame's own bytecode: `CompiledCode::expr_declared_syms` records a name
exactly when an expression-position declaration is a genuinely fresh binding,
deliberately excluding the one case where it is NOT — the synthesized
`WhateverCode` "promoted" declaration, which lexically belongs to the
surrounding source block and therefore MUST write through the enclosing
frame's cell (`roast/S02-types/whatever.t` #45, mirrored by
`t/expression-position-my-scope.t` #8). The two classes are complementary by
construction, so no new analysis or opcode was needed.

Both `SetGlobal` write-through sites now consult it:

- `src/vm/vm_exec_dispatch.rs`: the `SetGlobal` handler computes
  `fresh_binding_decl = self.vardecl_context && code.expr_declared_syms.contains(&Symbol::intern(&name))`
  before its ContainerRef write-through check, and skips that check when set.
- `src/vm/vm_env_helpers.rs`: the store then falls through to
  `set_env_with_main_alias`, which has its own, second write-through check
  reached after `vardecl_context` has already been cleared. A new dedicated
  entry point, `set_env_with_main_alias_fresh_binding`, is called instead of
  the general `set_env_with_main_alias` at the one call site that needs it —
  both delegate to a shared inner helper taking an explicit `fresh_binding:
  bool`, avoiding a new parameter on the general helper's ~95 call sites.

Gating on the ambient `vardecl_context` flag alone (without the
`expr_declared_syms` check) would have regressed exactly one thing: the
promoted `WhateverCode` declaration (`whatever.t` #45 /
`expression-position-my-scope.t` #8) — the single reason the compile-time set
has to be consulted rather than the flag alone.

Pin: `t/expr-decl-lexical-no-leak.t`, extended with all 8 method-dispatch
shapes from the probe matrix (each forcing the caller lexical into a shared
cell via an escaping closure first, then dispatching a method with a
colliding expression-position declaration).
