# A module's own non-exported top-level routine leaks into the loading scope

`require`-ing or `use`-ing a module whose own package-less top-level `sub`
declaration is NOT `is export`ed still leaves that sub permanently callable,
bare, from the loading scope after the load finishes — which raku does not do
(a package-less top-level `sub name {...}` is lexically scoped to its own
compilation unit).

```
# HelperMod.rakumod: sub helper() { say "mod helper" }
$ mutsu -e 'require HelperMod; helper();'
mod helper                      # raku: compile-time "Undeclared routine: helper"
```

## Attempted and blocked (2026-09-07) — read this before starting again

A full implementation exists and is **recoverable**: branch
`fix/module-private-toplevel-sub-compunit-scope`, PR **#7436** (closed, not
merged — `gh pr diff 7436` for the whole change, +556/-137). It is worth reading
rather than re-deriving; the design below is sound and only its last mile is
open.

### What it built

`runtime/unit_private_routines.rs`: seclusion is a **move, not a delete**. The
entry leaves the flat `GLOBAL::name` registry and enters
`Interpreter::unit_private_routines`, keyed by the declaring compunit;
`unit_private_routine` hands it back only to code compiled in that unit (or in
an `EVAL` nested inside it) — the same lexical-by-compilation-unit scoping
`user_declared_infix_ops` already uses, riding the `current_unit` symbol the VM
maintains.

The delete that every earlier attempt tried cannot work, and the branch says why
precisely: mutsu resolves a bare routine name through the flat registry, so the
module's **own** bodies — its exported subs, its classes' methods, its
`sub EXPORT` — reach their private helpers by the very key a sweep would delete.
(`import_module`'s existing non-exported sweep is gated on "shadows a core
builtin" for exactly that reason.)

Which routines are candidates is made **exact** rather than guessed:
`hide_toplevel_global_routines` already empties the package-less single-routine
namespace before the loaded body runs, so whatever occupies it afterwards was
registered by that body. Seclusion additionally skips `sub EXPORT`, multi
candidate slots, package-qualified entries, exported names (union of
`exported_subs` / `unit_module_exported_subs` / `module_owned_exports`),
`PRELUDE_SUB_TRAIT` routines (NativeCall's ambient `nativecast`/`nativesizeof`),
`MAIN`, and — added after roast caught it — `our sub name {...}`, which in a
package-less compunit really *is* a `GLOBAL` stash entry that the loading scope
legitimately reaches by bare name (discriminated by the pre-existing
`my_scoped_package_items` marker).

Two independent pre-existing bugs it fixed on the way, both worth salvaging
separately if this stays parked:

- the name-keyed call caches (`pos_light_call_cache`, `light_call_cache`,
  `otf_call_cache`, `func_multi_resolve_cache`, `fn_resolve_cache`) are keyed by
  `(name, package)` and cannot represent a winner that depends on which unit is
  asking;
- `call_compiled_function_positional_light_at`'s return-type-check failure path
  returned **without restoring `current_unit`** — a leak that also mis-scoped
  user-declared operators.

### What blocks it

The bundled-battery gate regressed 12 whitelisted files: ten `Cro::HTTP`
(`http-auth-basic`, `http-auth-basic-with-session`, `http-auth-webtoken-bearer`,
`http-auth-webtoken-cookie`, `http-log-file`, `http-middleware`,
`http-router-plugin`, `http-session-inmemory`, `http-session-persistent`,
`router-auth`), `IO::Socket::Async::SSL bad-incoming`, and
`zef distribution-depends-parsing`. Reproduced locally, identically.

The Cro symptom is `Unknown function: wrap-response-logging`, raised from
`Cro::HTTP::Middleware`'s

```raku
supply whenever wrap-response-logging($!middleware, $pipeline, {...}) -> $response { ... }
```

where `wrap-response-logging` is a package-less private top-level sub of that
very module.

Reduced to a self-contained probe (`t/lib`-style module plus a script):

```raku
# Mod.rakumod (package-less)
sub helper($x) { "helped-$x" }
sub tap-from-sub(Supply $in) is export { my @g; $in.tap(-> $v { @g.push(helper($v)) }); @g }
```

```
sub + .tap callback          -> Unknown function: helper     (raku: works)
method + .tap callback       -> Unknown function: helper     (raku: works)
supply/whenever body         -> QUIT: Unknown function: helper
plain block called directly  -> works
`start { helper(...) }`      -> works
`.map({ helper(...) })`      -> works
role method calling it       -> works
loading scope calling it     -> correctly refused
```

**So the blocker is one specific gap: a block handed to a NATIVE callback taker
does not enter its declaring compilation unit when it is finally invoked.**
`.tap` is such a taker, and every `supply`/`whenever` body is one. The closure
creation sites are innocent — `MakeAnonSub` and `MakeAnonSubParams` both already
stamp `source_file: self.executing_source_file()` — and
`call_compiled_closure_with_topic` already enters
`unit_of_source(data.source_file)`. The tap callback simply is not invoked
through that path: the supply machinery calls it through the generic code-object
entry (`call_sub_value` / `SupplierEmitAction::Call`), which never touches
`current_unit`.

### What closing it needs

Making the generic code-object invocation enter the callee's unit. That is a
real change, not a patch: `call_sub_value` is ~1000 lines with many early
returns (so it needs a save/restore wrapper, not an inline assignment), it is
the hottest generic call entry in the interpreter, and `current_unit` is *also*
what `user_declared_infix_ops` resolves against — so setting it there changes
operator scoping for every closure call in the program, which has to be measured
(full `make test`, full `make roast`, and the battery gate) rather than assumed.

Possible narrower shapes to weigh first:

- stamp the declaring unit on the code object itself (a `decl_unit: Option<Symbol>`
  on `SubData`, set from `executing_source_file()` at the two creation arms) and
  read it wherever a code object is invoked, instead of threading `current_unit`
  through the generic entry. 23 `SubData {` construction sites, but one central
  `Value::new_code_object` helper covers most of them.
- enter the unit only in the supply/tap emit path, which is where the measured
  regressions live — narrower, but leaves the same hole for every other native
  callback taker, so it would need a survey of those first.

**Do not restart from the sweep-and-delete design.** It is disproved, and the
branch above documents why.

## Gates any retry must pass

`make test`, full `make roast`, and — the one that caught this —
`scripts/battery-testsuite.sh`, run **alone** (two battery gates in a shared
workdir produce a false REGRESSION). The 12 files above are the acceptance set.
The pin for the feature itself is `t/module-private-sub-does-not-leak.t` with
`t/lib/PrivateSubMod.rakumod` (10 assertions, each verified against Rakudo), on
the closed branch.

## Related, filed separately

`todo/tickets/qualified-call-falls-back-to-bare-global-routine.md` —
`Pkg::name(...)` falls back to a bare `GLOBAL::name` even when `Pkg` does not
exist. Found while working this; independent of it.
