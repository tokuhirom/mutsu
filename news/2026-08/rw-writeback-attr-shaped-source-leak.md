# A method's exit-time `is rw`-writeback no longer plants attribute-shaped pseudo-keys in the caller's env

A named `:$scalar` parameter bound from an `@`/`%` argument that is itself an
attribute read (`:%!attr`, `:$.attr` — Slice 2d's "named scalar aliases an
array/hash source" sharing rule) encodes the caller-side source as that
attribute's own twigil form, e.g. `"%!plugin-config"`. The method's
exit-time `rw_writeback` step (`call_compiled_method` in
`src/vm/vm_method_dispatch.rs`) wrote that pseudo-key straight into the
caller's merged env, unconditionally, for every `rw_bindings` entry.

That pseudo-key is not a genuine lexical the caller can rebind — it is
reserved vocabulary that `reconcile_attrs`' `:=`-recovery candidate scan
(run at every *other* method's exit, to catch a real `$!x := $outer`
binding) also produces when checking a same-bare-name attribute of an
unrelated class. Once the writeback planted `"%!plugin-config"` in the
caller's frame, a later method call on a *different* instance sharing that
frame's env — one that also happened to have an attribute literally named
`plugin-config` (any sigil) — had its candidate scan find the pseudo-key,
mistake it for a `:=` override, and silently replace that instance's own
attribute with the unrelated one.

Real-world failure: Cro::HTTP::Router's `RouteSet.definition-complete`
calls `RouteHandler.copy-adding(..., :%!plugin-config, ...)` (RouteSet's
own `%!plugin-config` Hash attribute) once per included route handler. The
first call's writeback planted `"%!plugin-config"` in
`definition-complete`'s env; the *second* handler's own `$.plugin-config`
attribute (same bare name, different sigil, unrelated instance) then got
silently replaced with RouteSet's raw config on its own `.bless` inside
`copy-with`/`copy-adding` — `roast`-adjacent Cro::HTTP suite test
`http-router-plugin.rakutest` ("Local configuration in included route
handler not affected by outer") expected `i1,i2`, got `o1,o2`.

## Fix

`vm_method_dispatch.rs`'s `rw_writeback` loop now skips any `source_name`
shaped like an attribute-twigil env key (`!x` / `.x` / `@!x` / `@.x` /
`%!x` / `%.x`, via a new `is_attr_twigil_shaped` helper) before inserting
into the caller's merged env. The shared `ContainerRef` cell this writeback
exists for already keeps content mutations visible without the insert (see
`named_scalar_container_share_eligible` in `bind_function_args_values`), so
skipping attribute-shaped sources loses nothing.

`reconcile_attrs`' own candidate probe (`attr_env_or_local`) was also
tightened to be strictly overlay-only (never falling through to the parent
env chain), plus a same-call-entry-snapshot guard for the rare method whose
compiled code declares inner closures (which skips the usual scoped-overlay
isolation) — closing the adjacent hole the original diagnosis targeted,
even though the actual observed corruption traced to the `rw_writeback`
path above.

New regression pin: `t/method-rw-writeback-attr-source-no-leak.t`.

## Effect

- `http-router-plugin.rakutest` (Cro::HTTP suite, not in `roast/`): 7/7
  (was 6/7).
