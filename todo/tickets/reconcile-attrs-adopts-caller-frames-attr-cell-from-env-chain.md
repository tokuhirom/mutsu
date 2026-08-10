# `reconcile_attrs` adopts a CALLER instance's materialized attribute cell through the env chain, replacing the callee instance's same-named attribute

## TL;DR — fully diagnosed, fix is one function

`Interpreter::reconcile_attrs` (`src/vm/vm_method_dispatch.rs`) runs at
method exit to recover `:=`-bound attributes: for each attribute key of the
invocant's cell it probes candidate env/local keys (`bare`, `!x`, `.x`,
`@!x`, `@.x`, `%!x`, `%.x`) and, if the probe finds a `ContainerRef`,
commits that cell into the instance's attribute map (`commit_attrs`).

The hole: the cheap pre-check (`frame_has_container_ref`) is deliberately
**overlay-only** — its comment says a `:=` attr override "can only be
observed as a ContainerRef value in THIS frame's locals or env overlay" and
explicitly warns that "a caller-frame ContainerRef ... is NOT a binding
made by this method and must not be adopted as one". But the actual probe,
`attr_env_or_local`, falls back to `self.env().get(name)` — the **full env
chain**, caller frames included. The bare-name candidate has a guard
(`bare_owned`: must be a frame-owned slot, not a param, not a `my`), added
after the Cro::HTTP::Client `$timeout-policy` incident; the twigil
candidates (`!x`, `%!x`, ...) have NO guard, on the reasoning that "no
lexical can be called `!x`". That reasoning misses **another class's
method frame materializing ITS OWN attribute** under the same twigil key:
those keys leak to the callee through the flattened caller env whenever
two classes share an attribute name.

## Real-world failure (Cro::HTTP suite)

`t/http-router-plugin.rakutest` subtest 5, "Local configuration in included
route handler not affected by outer": expected "i1,i2", got "o1,o2".

In `Cro::HTTP::Router.rakumod`, `RouteSet` has `has Array
%!plugin-config{...}` and its inner class `RouteHandler` has `has
Hash[Array, ...] $.plugin-config` — same bare name, different sigils.
During `RouteSet.definition-complete` → `!generate-route-matcher` → (sub)
`compile-route($index, $handler)` → `$handler.signature`, the
`signature` method's exit reconcile probes candidate `%!plugin-config`,
finds the **outer RouteSet's** materialized `%!plugin-config` cell in the
inherited env chain, mistakes it for a `:=` binding made by `signature`,
and commits it into the included RouteHandler's attribute map — replacing
the handler's own (correct, `i1,i2`) config with the outer route block's
(`o1,o2`). Instrumented trace:

```
[RECON] attr=plugin-config via_key=%!plugin-config bare_owned=false routine=Some("signature")
GRM pre  compile-route[0]: ...RouteHandler|1767 pc=i1/i2
GRM post compile-route[0]: ...RouteHandler|1767 pc=o1/o2    # attr cell swapped
```

`copy-adding`/`bless` produce the correct value (verified: blessed
instance holds `i1,i2` right up to `compile-route`); only the
`signature` call's exit reconcile corrupts it.

## Repro

`tmp/router-plugin-diag.raku` (40 lines, against the Cro checkout under
`tmp/cro-work/`, run with the `inc-paths.txt` `-I` list + the HTTP dist
lib). mutsu prints `innermost: o1,o2`; raku prints `i1,i2`.

Four attempts at a Cro-free synthetic repro (two classes sharing an attr
name, sub-mediated cross-instance method call, Hash attrs, sigil mix —
`tmp/attr-writeback-cross*.raku`) did NOT reproduce: the leak needs the
caller's materialized twigil key to actually be visible in the callee
method's flattened env AND a ContainerRef in the callee frame to pass the
pre-check, a combination these small cases don't produce. Test the fix
against the real Cro file, not a synthetic.

## Fix direction

Make the probe match the pre-check's stated contract: restrict
`attr_env_or_local` to the frame's own locals and the env **overlay**
(born-owned tier), not the full chain — i.e. `self.env().overlay_get(name)`
or equivalent, for ALL candidate forms. A `:=` bind executed by this method
writes the override into this frame's overlay/locals, so legitimate
recoveries keep working; a caller frame's materialization becomes
invisible. Verify against the pins listed in the `reconcile_attrs`/
`bare_owned` comments: Cro::HTTP::Client request flow (`$timeout-policy`),
whatever pins cover `:=`-bound attributes (grep `t/` for `attr.*bind` /
`.VAR` on attributes), plus `bash tmp/cro-suite-run.sh http` —
`http-router-plugin.rakutest` should reach `notok=0` (its only failure as
of 2026-08-11).

## Context

Found 2026-08-11 while re-checking
`todo/tickets/deeply-qualified-exception-class-in-when-clause-...` (that
parse failure no longer reproduces; this attribute corruption is what
actually fails in `http-router-plugin.rakutest` now).
