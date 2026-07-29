# A deferred closure's captured lexical loses to a same-named caller lexical

A closure invoked through the **caller-priority env merge** (`call_sub_value(…,
merge_all = true)`) does not see its own captured value for any lexical the
*caller* happens to have declared under the same name. The caller's variable —
an entirely unrelated binding — shadows the capture.

```raku
our sub mkproxy($libname) is rw {
    Proxy.new(FETCH => -> $ { "saw:$libname" }, STORE => -> $, $ { })
}
sub caller-with() {
    my $libname = 'OUTER';          # same name, unrelated variable
    mkproxy('INNER')
}
say caller-with();    # raku: saw:INNER   mutsu: Use of uninitialized value
```

Note the mutsu result is *uninitialized*, not `'OUTER'`: the caller's `$libname`
lives in a local slot whose env twin is still unset, so the merge injects an
empty entry. When the caller's variable is a **parameter** (which does live in
env) the closure sees the caller's value instead — that is the shape that broke
`DBIish` (below).

Ordinary closure calls are correct — `-> { }` returned from a sub and invoked
normally sees `INNER`. Only the caller-priority path is affected, which in
practice means **`Proxy` `FETCH`/`STORE` bodies**: `maybe_fetch_rw_proxy` and
`auto_fetch_proxy` both call `call_sub_value(…, true)`.

## Why the caller-priority merge exists

`src/runtime/resolution_call_sub.rs` (~line 315) builds the callee env from the
caller env and then folds in the captured env:

- a captured `ContainerRef` **overwrites** — it is a shared cell, the single
  source of truth for that lexical;
- with `merge_all` the captured value is `entry_or_insert`ed — **the caller
  wins**;
- otherwise the captured value overwrites.

The `merge_all` arm is deliberate: `auto_fetch_proxy` documents that a `FETCH`
body must see the *current* value of a lexical its `STORE` twin mutates
(`substr-rw`'s `$str`). Freshness, not identity, is what it is buying.

## Why the obvious fix does not work

Preferring the capture for names in the compiler's `free_var_syms` (the
authoritative set of lexicals a body references from an enclosing scope) fixes
the repro above and the `DBIish` case, but breaks freshness where the merge was
load-bearing. Measured 2026-07-29: `NativeLibs`'s

```raku
multi sub cannon-name(Str:D $libname, Version $version?) {
    with $libname.IO { … $*VM.platform-library-name($_, :$version).Str }
}
```

froze `$version` at the first call's value for every later call — the `with`
block's captured env is persisted (`closure_env_overrides`) and stale, and only
caller-priority was keeping it fresh. So `try-versions` went from picking the
wrong library to picking none.

Name-based priority cannot separate the two cases, in either direction: "same
name" is not "same variable". The sound fix is identity — the shared case must
be a shared **cell** (the `ContainerRef` branch already does the right thing for
those), so `substr-rw`-style sharing stops depending on a name collision and the
`merge_all` caller-priority arm can be retired. That is the dual-store /
cell-ification work (ADR-0001 layer 3a, Track B), not a slice.

## Affected files

- `src/runtime/resolution_call_sub.rs` — the merge (~line 315-341); the
  `merge_all` arm is the defect site.
- `src/runtime/builtins_lvalue.rs` — `maybe_fetch_rw_proxy` /
  `auto_fetch_proxy`, the two callers that pass `merge_all = true` for a
  `Proxy` body.

## Minimal repro

`tmp/closurecap2.raku` in the working tree, or:

```raku
our sub mk($libname) { -> { "saw:" ~ ($libname // 'UNDEF') } }
our sub mkp($libname) is rw { Proxy.new(FETCH => -> $ { "saw:" ~ ($libname // 'UNDEF') }, STORE => -> $, $ {}) }
sub c1() { my $libname = 'OUTER'; mk('INNER')() }     # correct in mutsu
sub c2() { my $libname = 'OUTER'; mkp('INNER') }      # wrong in mutsu
say c1(); say c2();
```

## Impact

This is the **last blocker for `DBIish`'s real end-to-end mysql path**.
`NativeLibs::Searcher.try-versions('mariadb', 'mysql_init', 0..4)` probes each
candidate with `(try cglobal($cn, $wks, Pointer)) ~~ Pointer`. `cglobal`'s
`FETCH` closure captures `$libname`, and its caller `try-versions` has its own
`Str $libname` = `'mariadb'` — so every probe dlopens `mariadb` (which
`resolve_library_candidates` expands to the *existing* `libmariadb.so`) instead
of `libmariadb.so.<n>`. All five versions "succeed", `try-versions` returns
`libmariadb.so.0`, and the driver then fails to load it for real:

```
Cannot load native library 'libmariadb.so.0'
```

raku picks `libmariadb.so.3` and the whole script runs (verified against a live
MariaDB on 2026-07-29). A separate, now-fixed bug in the same path — an `is rw`
sub's `Proxy` result not being FETCHed on the OTF-compiled call branch — is
recorded in `news/2026-07/rw-sub-proxy-fetch-on-otf-call.md`.
