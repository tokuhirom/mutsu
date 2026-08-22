# A locally-declared `sub` does not shadow a same-named imported NativeCall sub

## Repro

`Compress::Zlib.pm6` (from the `Compress::Zlib` REA dist, v1.1.0) does:

```raku
use Compress::Zlib::Raw;   # exports `sub compress(Blob, CArray[long], Blob, ulong) returns int32 is native(...)`

our sub compress(Blob $data, Int $level = 6 --> Buf) is export {
    if $level < -1 || $level > 9 {
        die "compression level must be between -1 and 9";
    }
    _internal-compression($data, True, $level);
}
```

Under `raku`, a call to `compress($data)` inside this file resolves to the
**locally declared** `sub compress` (the 1-2-arg high-level wrapper), because a
lexically-scoped declaration in the same file shadows a same-named imported
symbol. Under mutsu it resolves to the **imported 4-arg NativeCall `compress`**
from `Compress::Zlib::Raw` instead:

```
$ mutsu -I lib -I ../Compress-Zlib-Raw/lib t/01-basic.t
1..5
ok 1 - Compiled
NativeCall: 'compress' expects 4 argument(s), got 1
  in block <unit> at t/01-basic.t line 10
```

Minimal shape (not yet reduced to a standalone repro without NativeCall —
reproducing needs two files: one that `is native(...) is export`s a sub named
`foo`, and a second that `use`s it and then declares `our sub foo(...) is
export { ... }` of its own, calling `foo(...)` from within its own body).

## Root cause (hypothesis, not fully confirmed)

mutsu's symbol resolution for a call inside a module body appears to prefer an
imported symbol over a same-named local declaration in the same lexical
scope, the reverse of Raku's own rule (own-file declarations shadow imports).
This is a general dispatch-resolution bug, not specific to NativeCall — the
NativeCall angle just makes the wrong-symbol case detectable (wrong arity).

## Where found

`docs/batteries/compression.md` survey (2026-08-22), measuring `Compress::Zlib`
(zlib/gzip compression battery candidate) — `t/01-basic.t`, `t/02-stream.t`,
`t/03-wrap.t` all fail this way; 0/3 files pass under mutsu vs 3/3 under raku.

## Affected files

- `src/compiler/` / `src/vm/vm_call_ops.rs` (function-call dispatch) — exact
  site not yet located.
