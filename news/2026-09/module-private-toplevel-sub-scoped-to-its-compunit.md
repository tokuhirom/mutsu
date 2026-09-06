# A module's non-exported top-level sub is now scoped to its own compunit

`require`-ing or `use`-ing a module whose package-less top-level `sub` is NOT
`is export`ed used to leave that sub permanently callable, bare, from the
loading scope:

```
# HelperMod.rakumod: sub helper() { say "mod helper" }
$ mutsu -e 'require HelperMod; helper();'
mod helper                      # raku: "Undeclared routine: helper"
```

Raku scopes such a routine lexically to its own compilation unit. mutsu
registered it as a shared `GLOBAL::helper` stash entry -- the same key the
loading scope's own package-less declaration would use -- and nothing
distinguished "exported, should reach the importer" from "merely declared,
private to this compunit". Filed as
`todo/deep/module-toplevel-private-sub-leak-cleanup.md`; this closes it.

## Why every earlier attempt failed: the fix is a MOVE, not a delete

The obvious cleanup -- after a successful load, delete every newly-registered,
non-`is export`ed package-less top-level routine -- was tried and reverted
(`news/2026-08/require-toplevel-routine-scoped-to-compunit.md`). It cannot
work, and the reason is not the exemption list it kept growing: mutsu resolves
a bare routine name through the flat registry, so the module's **own** bodies
-- its exported subs, its classes' methods, its `sub EXPORT` -- reach their
private helpers by the very `GLOBAL::helper` key the sweep deletes. The
existing `import_module` sweep for non-exported names says so out loud: it is
gated on "shadows a core builtin" precisely because "non-colliding helpers stay
in GLOBAL so the module's own bodies can still call them".

So the entry is **moved**, not deleted. `runtime/unit_private_routines.rs` takes
it out of the registry and into `Interpreter::unit_private_routines`, keyed by
the declaring compilation unit; `unit_private_routine` hands it back only to
code compiled in that same unit (or in an `EVAL` nested inside it). That is the
same lexical-by-compunit scoping `user_declared_infix_ops` already uses for
user-declared operators, riding the `current_unit` symbol the VM already
maintains across every compiled-routine call.

Which routines are candidates is made exact by machinery that already existed:
`hide_toplevel_global_routines` empties the package-less single-routine
namespace before the loaded body runs, so whatever occupies it afterwards was
registered by that body. Its predicate already excludes `sub EXPORT`, multi
candidate slots (additive across compunits by design) and package-qualified
entries. Seclusion additionally skips `our sub name {...}` -- in a package-less
compunit that really IS a `GLOBAL` stash entry and the loading scope
legitimately reaches it by bare name, which is exactly what the
`my_scoped_package_items` marker the registration path already maintains records
(`qualified_name_hidden_here`) -- exported names (the union of `exported_subs`,
`unit_module_exported_subs` and `module_owned_exports` --
conservative, so it can only under-fix), `PRELUDE_SUB_TRAIT` routines
(NativeCall's `nativecast`/`nativesizeof`/..., deliberately ambient in every
compunit that uses them, now recorded by name at their registration site), and
`MAIN`, which keeps its own narrower removal.

## What the audit found

Instrumenting the seclusion and running the whole `t/` suite named every
compunit that actually has private top-level routines: `Cro.rakumod`,
`Cro::HTTP::Middleware`, `Cro::HTTP::Router::LinkGenerator`,
`IO::Socket::Async::SSL` (its ~30 `my sub ...  is native(...)` bindings),
`Log::Timeline::Raku::Setup`, `OpenSSL::Digest`, `Text::CSV`, and the
`t/sub-export.t` fixture whose `sub EXPORT` selects among the module's own
private `greet-fr`/`greet-en`. All of them keep working: the `sub EXPORT` case
in particular runs *after* seclusion and still reaches those helpers, because
its body is compiled in the module's unit.

The `t/` suite did not, however, contain an `our sub` in a package-less module;
roast did (`roast/6.c/MISC/bug-coverage.t`'s `our sub module-transform`), and
CI caught it. That is the one exclusion the audit missed, and it turned out to
have an exact pre-existing discriminator rather than needing a new one.

## Two resolution gaps this surfaced

- **Name-keyed call caches.** `pos_light_call_cache` / `light_call_cache` /
  `otf_call_cache` / `func_multi_resolve_cache` / `fn_resolve_cache` are keyed
  by `(name, package)`, which cannot represent a name whose winner depends on
  the compilation unit asking. A private helper called once from inside its
  module was then answered from the cache for a *later* call in the loading
  scope -- the leak, re-entering through the front door. Such names now bypass
  those caches, exactly as `amp_param_shadowed_names` already does for a name a
  `&`-sigil parameter can shadow.
- **`current_unit` was not entered for method bodies.** `enter_compilation_unit`
  is called by the compiled-*sub* call paths only, so a method of a class the
  module declares ran with the caller's unit. `unit_private_routine` falls back
  to the innermost routine frame's `def_file` (what a backtrace renders) for
  those. Separately, `call_compiled_function_positional_light_at`'s
  return-type-check failure path returned without restoring `current_unit` at
  all -- a pre-existing leak that also mis-scoped user-declared operators; it
  restores now.

Pinned by `t/module-private-sub-does-not-leak.t` (9 assertions, all verified
against Rakudo) with its fixture in `t/lib/PrivateSubMod.rakumod`: the module's
exported sub, a method of a class it declares, and a block inside a module
routine all still reach the private helper; the loading scope's own same-named
routine is not displaced in either direction; and a private helper the loading
scope never declared is simply not there.

One divergence found along the way is filed separately rather than fixed here:
`Pkg::name(...)` falls back to a bare `GLOBAL::name` even when `Pkg` does not
exist (`todo/tickets/qualified-call-falls-back-to-bare-global-routine.md`).
