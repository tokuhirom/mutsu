# Upstream NativeCall vendored ahead of the switch, with a trial-load probe

Rakudo 2026.06's `lib/NativeCall.rakumod` and `lib/NativeCall/**` are now in
`modules/Rakudo-Core/lib/`, verbatim (md5s are in the README). They are inert
for now. `use NativeCall` and `use NativeCall::Types` are still served by the
native provider, and the files are not in `META6.json`'s `provides` until
ADR-11203's integration step (#11203) lands.

`scripts/nativecall-upstream-trial.sh` loads the vendored files under a renamed
namespace, because the real names are intercepted. It runs one probe per step
and reports where the real module stops. The first `FAIL` is the next slice:

- Upstream `NativeCall::Types` already loads, reports its native type traits
  and builds a `Pointer`.
- `CArray` construction fails. It needs the REPR work in #11209.
- `use NativeCall` itself fails. A user `trait_mod:<is>` candidate captures
  another compunit's traits (#11310).
