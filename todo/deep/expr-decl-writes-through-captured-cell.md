# Expression-position `my` writes through a captured caller lexical's cell

## Minimal repro (current main)

```raku
my $g = "outer";
sub f(*%args) { if (my $g = %args<g> :delete) { return 1 }; 42 }
EVAL q[f()];
say $g.raku;   # mutsu: Any — raku: "outer"
```

The caller's `$g` is declared BEFORE `sub f`, so `f` captures it as a free
variable and the capture machinery promotes it to a shared `ContainerRef`
cell. When `f`'s own expression-position `my $g` (declared inside the `if`
condition, so it compiles to the env-only `MarkVarDeclContext; SetGlobal`
store — no local slot) executes, the store finds the captured cell under the
bare env key and **writes through it**, clobbering the caller's variable with
the declaration's value (here `Any`, since `%args<g>` is absent). Declaring
the caller's `$g` AFTER the sub avoids the capture and the leak disappears —
that ordering is what `t/expr-decl-lexical-no-leak.t` pins as fixed.

## Why it is deep

This is the tail of the family fixed in the expr-decl-lexical-leak PR (the
free-var-write drain, the two return-merge exclusion gaps, and the carrier
write log are fixed there — see `news/2026-08/` entry of that PR). The
remaining path is the `ContainerRef` write-through itself, and it cannot be
gated shallowly:

- `compute_captured_mutated` (opcode.rs, the `expr_declared_syms` comment
  around the cell analysis) already EXCLUDES expression-declared names from
  earning the *caller's* local a shared cell, and notes: "The name stays a
  free var: the store still writes through to us, which is the pre-existing
  scope leak roast S02-types/whatever.t #45 pins." So the leak is known and
  load-bearing comments exist around it.
- The write-through happens in TWO places: the `SetGlobal` opcode handler's
  early `ContainerRef` branch (`vm_exec_dispatch.rs`, "Write through
  ContainerRef" — tried gating it on `!vardecl_context`; not sufficient
  alone) and `set_env_with_main_alias_sym`'s own cell check
  (`vm_env_helpers.rs` ~line 921), reached after the handler captured and
  cleared the flag.
- The correct rule is "a declaration shadows an INHERITED cell but a
  same-frame redecl keeps its own cell", which needs to distinguish
  frame-own vs inherited env entries. `Env::overlay_get` gives that for
  scoped-overlay frames, but several call paths run the callee on a FLAT
  cloned env (the `call_compiled_function_named_inner` merge iterates the
  whole env, not the overlay), where the distinction is lost. This is the
  scope-blind bare-name store again —
  [[bare-name-type-constraint-store-is-scope-blind]] is the same disease for
  constraint metadata.

## Suggested direction

Make the declaration store carry an explicit "fresh binding" signal all the
way into the env write (a parameter on `set_env_with_main_alias`, not the
ambient `vardecl_context` flag that gets consumed en route), and make the
fresh-binding write replace the env ENTRY (shadow) instead of writing through
a cell — then audit whatever.t #45 and pair.t #181, which pin today's
behavior at the two edges (capture-for-read must survive; unrelated later
`my` must get a fresh binding).
