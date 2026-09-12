# nqp::defined, nqp::isconcrete, nqp::list, and nqp::unshift

Four `nqp::` value ops that ordinary Raku modules reach for were
unimplemented, failing loudly with "Unsupported nqp:: op" instead of
silently aliasing to a different-semantics Raku builtin: `nqp::defined`,
`nqp::isconcrete`, `nqp::list`, and `nqp::unshift`. `nqp::unless` — the
fifth op the driving repro used — was already a compiled special form
(`compiler/nqp_forms.rs`); the real blocker was `nqp::isconcrete`.

- `nqp::defined` / `nqp::isconcrete` both collapse to the existing
  `runtime::types::value_is_defined` check: 0 for a type object or the VM
  null, 1 otherwise.
- `nqp::list` is the untyped VM list, using the same representation as the
  existing typed `list_s`/`list_i`/`list_n` (an ordinary array).
- `nqp::unshift` is the positional peer of `push_s`/`push_i`/`push_n`,
  inserting at the front of an nqp list / native array in place.

This unblocks Test::Async 0.1.17's `HubHOW` bundle registry, which builds
and reads a raw nqp list with exactly this idiom:

```raku
nqp::unless(nqp::isconcrete($bundle-typeobjs), ($bundle-typeobjs := nqp::list()));
nqp::unshift($bundle-typeobjs, bundle-typeobj);
```

See [#8024](https://github.com/tokuhirom/mutsu/issues/8024) and the
regression test `t/vm/nqp-defined-list-unshift-ops.t`.
