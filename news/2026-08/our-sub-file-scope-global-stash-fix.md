# `our sub` at file scope is now a real `GLOBAL::`/`OUR::` member

Found while narrowing `GLOBAL::`/`OUR::` stash enumeration (see
`news/2026-08/global-our-stash-enumeration-narrowing.md`): `our sub baz {}` at
file scope did not expose `&baz` in `GLOBAL::.keys`/`OUR::.keys`, while rakudo
does. An under-inclusion bug, the opposite of the over-inclusion the sibling
change closed.

## Root cause (confirmed with `rust-gdb -batch`, not guessed)

Neither of the ticket's two first-guess candidates was right:

- The registry key for a 0-arg (non-multi) `our sub` at the top level carries
  **no** candidate/signature suffix at all -- breaking on the
  `registry().functions` loop body in `package_stash_value`
  (`src/runtime/accessors_stash.rs`) and printing `key_s` showed the raw key
  is simply `"GLOBAL::baz"`, matching every other top-level sub regardless of
  scope (`"GLOBAL::foo"` for a plain `sub foo {}`, `"GLOBAL::bar"` for `my sub
  bar {}` too).
- `is_my_scoped_package_item` was not misclassifying anything -- it was never
  given the chance to run.

The actual bug: `stash_member_tail(key, "GLOBAL")` has a special case that
returns the **whole key unconditionally** for the `GLOBAL` package (there is
no `"GLOBAL::"` prefix to require, unlike a named package). The env,
classes, and roles loops in the same function already know this and strip a
leading `"GLOBAL::"` self-qualification before using the tail (added by the
sibling over-inclusion fix). The `functions` loop (and the `proto_functions`
loop right after it) did not do this stripping. So for every top-level sub,
`rest` was the full `"GLOBAL::baz"`, and the loop's own
`base.contains("::")` guard -- there to reject a tail that names a
`Sub::Package`-qualified member, not a plain name -- fired and skipped the
entry. This affected *every* top-level sub identically, so it was silently
compensating for a **second**, previously-latent bug: nothing ever marked a
top-level (GLOBAL-package) plain `sub`/`my sub` as lexical
(`mark_my_scoped_package_item` was gated behind `self.current_package() !=
"GLOBAL"`, and `register_proto_decl`'s `mark_our_scoped_package_item` call
behind the same guard), because the `"::"` bug already hid all of them
regardless of scope, so nobody needed the real marker. Fixing only the
prefix-stripping without also fixing the guard would have made the negative
controls (plain `sub`/`my sub` at file scope) start leaking into `GLOBAL::`
too.

## The fix

Two changes, both required together:

- `package_stash_value`'s `functions` and `proto_functions` loops now strip a
  leading `"GLOBAL::"` self-qualification from the registry key before
  calling `stash_member_tail`, exactly like the env/classes/roles loops
  already do.
- `registration_sub.rs` no longer skips `mark_my_scoped_package_item` /
  `mark_our_scoped_package_item` when `current_package() == "GLOBAL"` -- a
  top-level `sub`/`my sub` is exactly as lexical as one inside a named
  package, and the root package deserves the same scope bookkeeping as any
  other.

## Surface measured against rakudo (v2026.06)

| Case | `GLOBAL::`/`OUR::` member? |
|---|---|
| `our sub baz {}` at file scope | Yes, `&baz` |
| `our sub baz(Int $x) {}` (with parameters) | Yes, `&baz` |
| `our proto sub baz(\|) {*}` + two `multi sub baz(...)` candidates | Yes, `&baz` exactly once (not per candidate; raku itself rejects `our multi sub` on an individual candidate -- "Please declare an our-scoped proto instead") |
| `module M { our sub baz {} }` | `&baz` in `M::.keys`; `M` itself (not a flat `&baz`) in `GLOBAL::.keys` |
| `our $x` / root-scope `our` variables | Yes (already fixed by the sibling change; re-verified, not regressed) |
| plain `sub foo {}` at file scope | No (negative control) |
| `my sub bar {}` at file scope | No (negative control) |
| `GLOBAL::<&baz>` / `::('&baz')` | Both retrieve the actual callable routine, not just a listed key -- calling it runs the real body |

One pre-existing, unrelated divergence was found and left alone: rakudo
rejects the bareword call syntax `GLOBAL::baz()` outright ("Could not find
symbol 'baz' in 'GLOBAL'") for *both* `our`- and plain-scoped subs alike --
only the `&`-sigiled form (`&GLOBAL::baz()`) or a stash subscript
(`GLOBAL::<&baz>()`) actually calls it. mutsu still accepts the bareword form
for both cases (no change from before this fix); that is a separate,
narrower gap in the qualified-call dispatch path, not the stash-membership
question this ticket was about.

## Tests

Extended `t/eval-compunit-introspection.t`'s `GLOBAL::`/`OUR::` section with
11 new assertions covering the table above (`plan` raised from 62 to 73). All
73 assertions pass verbatim under both `raku` and `mutsu`.

`t/bare-package-symbolic-deref.t` stays green. The roast baseline the sibling
change recorded is unchanged: `roast/6.c/S02-names/pseudo-6c.t` still fails
exactly its recorded 14/161. Targeted sweeps of the whitelisted
`S02-names`/`S02-names-vars`, `S10-packages`, and `S11-modules` files (stash
enumeration and symbolic lookup's actual consumers) all still pass.
