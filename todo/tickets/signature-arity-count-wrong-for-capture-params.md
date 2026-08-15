# `Signature.arity`/`.count` are wrong for any signature containing a raw-capture (`|c`) param

Found while implementing ADR-0019 Phase F box F1's mechanism-slice `.signature` default for native
methods (`todo/deep/adr0019-f1-f2-introspection-canonical-source.md`). Not caused by that change --
reproduces on a plain user-declared sub, so it is a pre-existing, general bug in
`src/value/signature.rs`'s arity/count computation, not specific to native methods.

## Repro

```
$ raku -e 'sub foo(|c) {}; say &foo.signature.arity; say &foo.signature.count;'
0
Inf
$ ./target/debug/mutsu -e 'sub foo(|c) {}; say &foo.signature.arity; say &foo.signature.count;'
1
1
```

Real Rakudo: a raw capture (`|c`) contributes 0 to `.arity` (it captures whatever remains, not a
required positional) and makes `.count` unbounded (`Inf`), since the signature accepts any number of
further arguments. mutsu counts the capture param itself as one required, bounded parameter.

## Where to look

`src/value/signature.rs` -- wherever `.arity`/`.count` are derived from `SigInfo`/`SigParam` (likely
near `sig_param_to_parameter_instance`/`build_parameter_attrs` or a dedicated arity-computation
helper). The fix is general: any `SigParam` with `is_capture: true` present in `params` should make
`.count` answer `Inf` and should not itself count toward `.arity`.

## Why not fixed inline

Out of scope for the F1 mechanism slice that found it (that slice only needed `.signature` to exist
and gist-render sensibly for native methods, not exact `.arity`/`.count` fidelity). Fixing arity/count
touches the general signature-introspection surface for every capture-taking sub/method (user and
native), which is a distinct, self-contained fix better landed and tested on its own.
