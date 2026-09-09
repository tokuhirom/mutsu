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
private to this compunit". Filed as #7558; this closes it.

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
  to [`Interpreter::executing_unit_sym`] -- the file baked onto the innermost
  enclosing routine frame, and the same anchor `prelude_visible_here` already
  uses for the identical question about NativeCall's prelude splices.

## The blocker that parked the first attempt: a code object with no file

The first attempt at this (PR #7436) was closed because it regressed twelve
whitelisted battery files -- ten `Cro::HTTP`, `IO::Socket::Async::SSL
bad-incoming`, and `zef distribution-depends-parsing` -- all with `Unknown
function: wrap-response-logging`, a package-less private sub of
`Cro::HTTP::Middleware` that the module's own `supply whenever ...` body calls.
Reduced: **a block handed to a native callback taker could not reach the private
routines of the compunit it was written in.** `.tap` is such a taker, and so is
every `supply`/`whenever` body.

The cause was not the invocation path, as first diagnosed, but the code objects
themselves: two whole classes of closure did not record the file they were
written in, so nothing downstream could tell which compunit they belonged to.

- **`MakeLambda` / `MakeBlockClosure`** (`vm/vm_register_sub_ops.rs`) stamped
  `current_source_file()` -- the dynamically-scoped `?FILE`, which tracks the
  unit being *loaded* and has long since reverted to the caller's file by the
  time an already-loaded module's routine runs and rebuilds its closure literal.
  They now use `executing_source_file()`, exactly as the sibling `MakeAnonSub` /
  `MakeAnonSubParams` arms already did.
- **`whenever` callbacks** are built at runtime from AST by
  `run_whenever_with_value` (`Value::make_sub_owning`), which records no source
  file at all. They are now stamped with the file the body was written in, via
  the new `Interpreter::sub_with_source_file`.

The third piece is the frame: `call_sub_value`'s block-carrier path -- how every
block handed to a native callback taker is actually invoked -- pushed a
`RoutineFrame` with `def_file: None`, so even a correctly-stamped closure went
unnoticed. It now records `data.source_file`, exactly as the compiled closure
dispatch (`vm_closure_dispatch.rs`) already did.

All three are corrections in their own right: `executing_unit_sym` is what
`?FILE`-based backtrace attribution, `callframe`, `%?RESOURCES` lookup and
NativeCall prelude visibility all read.

Separately, `call_compiled_function_positional_light_at`'s return-type-check
failure path used to return without restoring `current_unit` at all -- a
pre-existing leak that also mis-scoped user-declared operators. That one was
salvaged and landed on its own ahead of this change.

Pinned by `t/module-private-sub-does-not-leak.t` (12 assertions, all verified
against Rakudo, which passes the file unchanged) with its fixture in `t/lib/PrivateSubMod.rakumod`: the module's
exported sub, a method of a class it declares, and a block inside a module
routine all still reach the private helper; a `.tap` callback and a
`supply`/`whenever` body written in the module reach it too, dispatched later
from the main script; the loading scope's own same-named routine is not
displaced in either direction; and a private helper the loading scope never
declared is simply not there.

One divergence found along the way is filed separately rather than fixed here:
`Pkg::name(...)` falls back to a bare `GLOBAL::name` even when `Pkg` does not
exist (#7709).
