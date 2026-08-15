# `t/native-array-storage.t` fails deterministically in release builds only

## Repro

```
cargo build --release
timeout 30 target/release/mutsu t/native-array-storage.t
```

Fails deterministically (3/3 runs) on subtest 6:

```
not ok 6 - C writes through retained payload are visible in Raku
# expected: '42'
#      got: '20'
```

The debug build (`cargo build && target/debug/mutsu t/native-array-storage.t`)
passes all 8 subtests every time.

## Confirmed pre-existing on `main`

Built `origin/main` in a separate worktree (`cargo build --release`) and ran
the same test: identical deterministic failure (3/3), same expected/got
values. This is **not** a regression from any in-flight branch work — it
reproduces on `main` at commit `22e0fbd32580fb40d0d145a2961b552780bec3d3`
(2026-08-16) under release optimization only.

## What the test does

```raku
class MVMArrayB is repr('CStruct') {
    has uint64 $.elems;
    has uint64 $.start;
    has uint64 $.ssize;
    has Pointer $.any;
}
my int @a = 10, 20;
my $body = nativecast(MVMArrayB, Pointer.new(@a.WHERE));
my $payload = nativecast(CArray[int64], $body.any);
$payload[1] = 42;
is @a[1], 42, 'C writes through retained payload are visible in Raku';
```

`@a.WHERE` gives the native array's backing-storage address; the test casts
it to a raw `CArray[int64]` and writes through the pointer directly,
expecting the write to be visible through `@a` afterward (this is exactly
how NativeCall out-parameters / shared native buffers are expected to work).
Under a release build, the write does not appear to reach the storage `@a`
subsequently reads from — `@a[1]` still reports the original value (20)
after the raw pointer write set it to 42.

## Why this is release-only (hypothesis, not confirmed)

Likely candidates, not yet root-caused:
- A COW/rebuild path for `int @a`'s backing storage that release-mode
  inlining/reordering causes to run (or skip) differently around the
  `nativecast`/`Pointer.new(@a.WHERE)` call, so the pointer handed to C code
  does not alias the storage `@a[1]` later reads.
- Optimizer assuming no-aliasing between the native array's Rust-side buffer
  and the raw pointer obtained via `.WHERE`, if the buffer isn't accessed
  through a volatile/raw-pointer-aware path on the read side.

## Scope note

Not investigated further this session — found as a side effect of a release
build sweep for PR #6499 (unrelated tail-call/hash-metadata fixes). Needs a
NativeCall/`.WHERE`/native-array-storage-focused investigation using
`rust-gdb` on the release binary (per CLAUDE.md's debugging guidance) to
find exactly where the write-through path diverges between debug and
release.
