# `%?RESOURCES` no longer loses its own distribution to a second `use`

Any `unit module` that made more than one `use` statement before reading its
own `%?RESOURCES` lost the resource list entirely: `%?RESOURCES<name>` came
back `Any` and the follow-on `.slurp` blew up with "No such method 'slurp'
for invocant of type 'Any'", wrapped unhelpfully as "An exception occurred
while evaluating a CHECK". `Net::Netmask`'s and `LLM::Prompts`'s
`use JSON::Fast; use XDG::BaseDirectory :terms;` shape (and eight other
distributions listed against #8004) hit this at load time and failed to
import at all.

## Root cause

`load_module_inner` records which distribution owns a loading module's
`%?RESOURCES` under two keys in `package_distributions`: the module's own
declared name, and "the current runtime package" — meant to cover a bare
file with no `unit module` declaration, whose top-level subs compile under
the generic `GLOBAL` package rather than their own name.

That second key was read via `self.current_package()` at the moment each
module's *own* load began. For a *nested* `use` — a dependency loaded from
inside another module's still-executing mainline — that ambient value is
wrong: it reflects whatever the *importer's own* `unit module` declaration
had already set `current_package` to, not the dependency's future package.
So loading the dependency stamped `package_distributions[<importer's own
package>]` with the dependency's distribution, clobbering the importer's own
correct entry the moment it made a second `use`.

`%?RESOURCES` resolution first tries a more precise, collision-immune route
(the calling routine's own declaring source file, via `SubData::source_file`
walking up to the nearest `META6.json`), but that route requires the
`def_file` field to be populated — which it is not for a plain top-level
named `sub` invoked from a module's own `BEGIN` block (the exact shape
`ingest-prompt-stencil()`/`ingest-prompt-data()` use in `LLM::Prompts`, and
`Net::Netmask`'s `ip2arr`/`ipmask2arr`). Resolution then fell through to the
corrupted `package_distributions` entry.

## Fix

Compute the "current runtime package" key from a static fact of the file
being loaded instead of the ambient dynamic package: `detect_unit_package_name`
already tells us whether the module declares its own `unit module`/`unit
class`/`unit package`, so the correct key is that name if present, or
`"GLOBAL"` otherwise — never a value read from whatever package the
*importer* happens to be running under. This is entirely independent of
nesting, so a dependency loaded from inside another module's mainline can no
longer stamp the wrong key.

A regression test lives at `t/modules/resources-second-use-module.t`, backed
by two fixture distributions (`t/lib/ResSecondUse/`, its own `unit module`
that `use`s a second fixture distribution before reading its own
`%?RESOURCES` from a `BEGIN`-invoked sub) — the minimized shape of the
`Net::Netmask`/`LLM::Prompts` failure, verified to fail without the fix and
pass with it.

Fixes #8004. `LLM::Prompts` now gets past its own module load (reaching a
separate, unrelated bug further into its own logic); the remaining eight
distributions listed against the issue were not individually re-verified in
this PR.
