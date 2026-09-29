# ADR-0132: An `is export` routine nested in a routine body is exported when the enclosing routine is installed

- **Status**: Accepted (2026-09-29); implemented in the same PR as the decision.
- **Resolves**: [#10050](https://github.com/tokuhirom/mutsu/issues/10050)
- **Related**: [ADR-0113](0113-frame-lexical-inner-subs.md) (frame-lexical inner subs; an
  exported declaration is never one), [ADR-0114](0114-routine-nested-sub-free-var-aliases.md)
  (how a routine-nested sub reads its free variables)

## 1. Problem

```raku
unit module Nest;
multi sub set(Callable $c) is export {
    my @t;
    my multi sub test(Str $d, Callable $s) is export { @t.push($d) }
    my multi sub test(Callable $s) is export { test("anon", $s) }
    $c();
    say @t;
}
# importer:  use Nest; set(sub { test("a", sub {}); test(sub {}) });   # raku: [a anon]
```

Rakudo runs the `is export` trait at compile time, so `EXPORT::DEFAULT` holds `&test` before
anything calls `set`. When `test` runs during `set`'s dynamic extent, MoarVM binds its outer to
`set`'s live frame, found on the caller chain.

mutsu registered a nested declaration only when its `RegisterDecl` executed, i.e. inside a call
of `set` — after the importer's `use` had copied the export table, and outside any module load,
so the registration did not reach the export table either. The importer saw
`Unknown function: test`. (The `Green` distribution's `t/01-time.t` is this shape.)

## 2. Decision

**Installing a routine during a module load also registers every `is export` routine declared
in its compiled body** (`Interpreter::register_nested_exported_subs`, called from the
`Installed` branch of `exec_register_sub_op_in_registry`).

- The enclosing routine's installation is the earliest point where both the nested
  declaration's plan (`CompiledSubDeclPlan`, in the enclosing body's `sub_decl_plans`) and its
  compiled body (in the same `CompiledFns`) exist. Running the ordinary registration on that
  plan performs every check and export step the in-sequence declaration would; the nested
  declaration's own later execution is the idempotent re-registration.
- The pass recurses naturally: a nested routine that is itself installed here registers its
  own nested exports.
- It runs only while a module loads (`module_load_in_progress`) and exports are not
  suppressed. Outside a load there is no importer to hand the routine to, and registering it
  early would only make it callable outside its lexical scope.
- The hoist pre-pass copy (`__hoisted`) and a computed-name declaration (`name_chunk`) are
  skipped: the former is a stripped duplicate of the in-sequence plan, the latter needs its
  name expression evaluated in the enclosing frame.

**The exported object is the static routine; its free variables need no new mechanism.** A
named routine reads a free variable in the env live at the call, and a callee runs in an
overlay of its caller's env. A call made during the enclosing routine's dynamic extent
therefore sees that routine's frame through the caller chain — Rakudo's "autoclose" falls out
of it. A non-`multi` nested sub additionally reads through the ADR-0114 per-activation aliases,
which live in the enclosing frame and so are reachable the same way.

## 3. Rejected alternatives

- **Hoist the nested declarations into the compunit's mainline at compile time.** The
  mainline would need its own compiled copy of each nested body, compiled outside the scope
  that declares its free variables; the enclosing routine's plan already holds the right one.
- **Extend `preregister_inline_package_subs` / the forward-declaration pass to walk routine
  bodies.** That pass works from the AST, before the mainline is compiled, and would register
  a second, separately compiled body next to the one the enclosing routine's declaration
  registers later.
- **A per-activation code object that resolves its outer by searching the caller chain for
  the declaring routine's frame.** It gives stricter semantics for a call made *outside* the
  dynamic extent (§4), at the cost of a new closure kind that every call path and the JIT would
  have to learn. Nothing observed so far depends on that case.

## 4. Consequences and limits

- A module load pays one scan of `sub_decl_plans` per installed routine; the scan is over
  declarations already compiled, and exported nested ones are rare.
- The nested routine is callable by its bare name from other code in the module's package once
  the module is loaded, not only inside its lexical scope. Rakudo rejects such a call at
  compile time; mutsu has no lexical routine scope at the package level to reject it with.
- **A call outside the enclosing routine's dynamic extent** originally read the free variable
  by name in the caller's env: `my @t; test("x", sub {})` in the importer pushed onto the
  *importer's* `@t`. Resolved by
  [#10114](https://github.com/tokuhirom/mutsu/issues/10114) without the rejected per-activation
  code object: the routine-nested sub's latest-activation cells (`lexsub_latest_cells`, the
  `capturelex` emulation of ADR-0114's aliases) now carry the owning routine's package and
  defining file, and an entry whose owner matches the running routine wins over the caller's
  same-named binding. Installing the enclosing routine during a module load seeds each exported
  nested sub's entries with a fresh container per free variable, shared by the sibling subs of
  that routine — the static outer frame. So a call before any activation reads that static
  frame, and one after the routine returned reads its latest activation. Rakudo additionally
  lets a write to the static frame show up in the next activation (a MoarVM artefact); mutsu
  starts every activation fresh.
