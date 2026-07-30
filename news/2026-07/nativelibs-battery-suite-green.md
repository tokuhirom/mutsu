# `NativeLibs`' upstream test suite passes in full

`NativeLibs` (0.0.9, 96 dependents in the fez index) is bundled as a runtime
dependency of the `DBIish` database battery, but none of its four upstream test
files was in the release gate's baseline: only 1 of the 9 files across it and
`NativeHelpers::Blob` passed. All four `NativeLibs` files now pass, and so does
`NativeHelpers::Blob`'s `00-trivial.t` — the gate's baseline goes from 132 to 137
files (closing [#5558](https://github.com/tokuhirom/mutsu/issues/5558), and
[#5557](https://github.com/tokuhirom/mutsu/issues/5557) as far as it can go).

Every fix below is a general interpreter or compatibility fix, not a
`NativeLibs`-shaped patch — rung 2 of the
[adoption policy](../../BATTERIES.md#1-adoption-policy--community-first-adopt-as-is).

## `$*VM.config` was nearly empty

It carried three keys (`name`, `be`, `nativecall_backend`). Missing among them
was **`osname`**, which `NativeLibs` uses to switch its entire library-naming
scheme:

```raku
given $*VM.config<osname>.lc {
    when 'linux'|'freebsd' { ... }
    when 'darwin'          { ... }
}
```

An undefined key matched nothing, so on Linux the whole platform section was
skipped in silence: `02-cannon-name.t` planned 10 tests and ran zero. Also
missing was the **C toolchain** MoarVM records (`cc`, `cflags`, `ccshared`,
`ccout`, `obj`, `ld`, `ldshared`, `ldflags`, `ldlibs`, `ldout`, `dll`), which
`NativeLibs::Compile` joins into a `shell()` command line to build a companion
`.so` for a binding.

mutsu is not built by a C compiler the way MoarVM is, so there is no recorded
build config to echo back. `src/runtime/io_sysinfo_vm_config.rs` reports a
working *host* toolchain instead — which is what the consumers actually want the
keys for — and lets `CC` / `LD` / `CFLAGS` from the environment win, so an
alternate toolchain can still be selected. Pinned by `t/vm-config-toolchain.t`,
which also asserts that `config<dll>` (a sprintf pattern) agrees with
`$*VM.platform-library-name`.

## `platform-library-name` mangled any path with a directory

It decorated the whole string rather than the basename, so `/bar/foo` became
`lib/bar/foo.so` instead of `/bar/libfoo.so`. Rakudo decorates the basename, puts
the directory back, and makes the result absolute whenever the input carried any
directory at all (`./foo` → `$*CWD/libfoo.so`) — which is exactly what makes the
"load the `.so` I just built next to me" idiom work. Pinned by
`t/platform-library-name-path.t`.

## Package-qualified multi dispatch ignored optional parameters

```raku
module M {
    our proto sub f(|) { * }
    multi sub f(Str $l, Version $v = Version) { ... }
    multi sub f(Str $l, Cool $c) { ... }
}
M::f('foo');   # No matching candidates for proto sub: M::f
```

Candidates are registered under keys built from their *declared* arity, so a call
that omits a defaulted trailing parameter never matches the exact-arity keys. The
bare-name resolution path has had a fallback for this for a long time; the
package-qualified path did not, so a qualified call could not reach a candidate
that the identical bare call resolved fine. Both qualified paths
(`resolve_function_with_types` and `resolve_proto_candidate_with_types`) now share
one `qualified_flexible_arity_candidates` fallback.

## A native sub called through a code object ran its `{ * }` stub

`NativeLibs` picks between the dyncall and libffi symbol lookups by *value*:

```raku
with (is-win ?? &GetProcAddress !! dyncall ?? &dlFindSymbol !! &dlsym)(|c) { ... }
```

The two VM call opcodes check the `is native` registry by name, but a call
through a code object resolves the callee as a value and never consulted it — so
the sub ran its literal `{ * }` body and returned the Whatever `*`, failing its
own return-type check. `call_sub_value` and `vm_call_on_value` now try the same
name-keyed native dispatch first.

## `nativecast(<Signature>, $ptr)` was not implemented

Attaching a signature to a function pointer obtained at runtime is the only way a
`dlsym`ed symbol becomes callable:

```raku
$dll.symbol('sin', :(num64 --> num64))(pi / 2);   # 1
```

`src/runtime/nativecall_fnptr.rs` builds a `NativeCallSpec` from the signature and
registers it under a synthetic key, so every existing name-keyed dispatch path
handles the result with no further plumbing; `NativeCallSpec` gained an `entry`
field that bypasses library/symbol resolution. Both routes are pinned by
`t/nativecall-signature-cast.t`.

## `constant Foo = Int` was not usable as a type

C bindings spell their platform types this way — `constant HANDLE = uint32;
sub GetProcessHeap(--> HANDLE) is native('kernel32') { * }` — and Raku accepts
the alias anywhere a type name goes. mutsu's compile-time signature validator
rejected it outright ("Invalid typename" / "Type 'HANDLE' is not declared"), so
the whole declaration failed to compile. The alias is now recognized, but only
when it points at some *other* resolvable type: a `package` binds its own name to
itself, and `my package A {}; sub foo(A $a)` must stay
`X::Parameter::BadType`. Pinned by `t/constant-type-alias.t` (including all three
negative cases).

## `our proto sub` was invisible in its package's stash

```raku
module M { our proto sub f(|) { * }; multi sub f(Int) { 1 } }
M::.keys          # was ()          raku: (&f)
::('M::&f')       # was a Failure
```

Two causes. Protos live in their own registry, which the package-stash builder
never scanned; and each bare `multi` candidate marked `M::f` `my`-scoped, which
hid the name however the proto was declared. A proto is the one *visible* name of
a multi (its candidates are lexical), so `our` on it publishes the whole routine:
`Stmt::ProtoDecl` now carries `is_our`, and an explicit `our` marking wins over
the `my` marking of the same name. A bare (non-`our`) proto still stays lexical,
matching Rakudo. Pinned by `t/our-proto-package-stash.t`.

## `NativeCall`'s export list was empty

mutsu implements NativeCall inside the VM, so `use NativeCall` loads no Raku
module — but the module's export surface is introspectable in Rakudo, and
`NativeLibs` copies the whole `NativeCall::EXPORT::ALL` stash into its own
`UNIT::EXPORT` so that its users get NativeCall transitively. With an empty stash
that re-export silently did nothing. Three gaps closed along the way, all
general:

- `NativeCall`'s export names are registered (`register_nativecall_exports`), and
  `::('NativeCall')` resolves to the package.
- A module that exports anything gains an `EXPORT` member in its own stash, and
  an EXPORT package always lists the `ALL` tag. Without those, walking
  `Mod::EXPORT::ALL` one component at a time failed even though the whole name
  resolved.
- `NCexports::{$_}` on a lexical bound to a package now reads *that package's*
  stash rather than looking for a package literally named `NCexports`.

Pinned by `t/nativecall-export-stash.t`.

## `:v<…>` was not accepted as short for `:ver<…>`

`use-ok 'NativeLibs:v<0.0.9>'` could not find the module: only `ver`, `auth` and
`api` were recognized as distribution selectors, so `:v<0.0.9>` was taken as part
of the module *name*. Pinned by `t/use-version-short-adverb.t`.

## A `when` block's value was warned about as sink context

`when`/`default` succeed out of the enclosing topicalizer with their final
statement's value, so it is not sunk — Rakudo warns for `if True { 1 }` but not
for `when 'a' { 1 }`. mutsu warned for both, and because the analysis is
compile-time the warning fired even on a branch that never runs: a Linux-only
test file emitted a spurious "Useless use of constant string … in sink context"
for the `when 'darwin' { skip-rest, "Tests missing" }` arm it skipped. Statements
*before* the last one are still sunk. Pinned by
`t/when-block-value-not-sunk.t`.

## Gate harness: a `# TODO` failure is not a failure

`NativeLibs`' `10-search.t` marks its "is there a versioned `libmysqlclient`?"
probe TODO, and raku fails that subtest on this machine too. TAP (and `prove`)
treat a `not ok … # TODO` as an expected failure; the battery gate counted it as
a real one, which made the file ungateable at exact parity with raku. The harness
now excludes TODO failures from its verdict and reports the count
(`PASS(1 todo)`), so a file quietly turning its whole plan into TODOs stays
visible.

## What is still blocked

`NativeHelpers::Blob`'s `01-basic.t` (8/24) and `03-pointer.t` (0/10) both stop at
the same wall: a `CArray[T]` constructed from Raku has no C storage, so its
`.REPR` is `P6opaque` and it has no address to `nativecast` from. That is
**ADR-0015 P3** (native-backed Raku-side `CArray[T]` / `array[T]`), a designed but
unstarted phase — see
[`todo/deep/nativehelpers-blob-moarvm-guts.md`](../../todo/deep/nativehelpers-blob-moarvm-guts.md),
which now records exactly which files and subtests it gates. `02-cstruct.t` is
not whitelistable at all: raku itself fails two of its tests here.

One narrower fix landed in this area anyway, because `NativeHelpers::Blob`'s
signatures need it: an unparameterized `CArray` constraint now accepts a
`CArray[T]` — parameterization narrows a type rather than replacing it, so
`isa-ok $au, CArray` and `sub carray-is-managed(CArray:D \arr)` both work.
Pinned by `t/carray-base-type-match.t`.
