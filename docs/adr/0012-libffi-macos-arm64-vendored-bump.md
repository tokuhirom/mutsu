# ADR-0012: libffi on macOS arm64 — bump the vendored build, do not switch to system libffi

- **Status**: Accepted
- **Date**: 2026-07-19
- **Deciders**: tokuhirom, Claude
- **Related**: `.github/workflows/release.yml` (the release build matrix), `src/runtime/nativecall.rs`
  (the sole libffi consumer), CLAUDE.md "Distribution" note.

## 1. Context

mutsu's NativeCall support (`src/runtime/nativecall.rs`) calls C functions through libffi, pulled in
via the Rust `libffi` crate (feature-gated behind `native`/`libffi`). Until this ADR the manifest
pinned `libffi = "3"`, which resolves to:

- `libffi` 3.2.0 (high-level, `middle`/`low`/`raw` bindings)
- `libffi-sys` 2.3.0, which **vendors and builds the libffi C source in-tree** (libffi ~3.4.x)

Since v0.3.1 **every tagged Release silently shipped no macOS binaries.** The macOS jobs in
`release.yml` are `continue-on-error: true`, so a build failure there does not fail the workflow —
the Linux tarballs publish and the missing macOS tarballs went unnoticed. Root cause: the vendored
libffi 3.4.x `src/aarch64/sysv.S` fails to assemble under Apple Clang 16/17+ with
`error: invalid CFI advance_loc expression`. This is upstream libffi issue
[#852](https://github.com/libffi/libffi/issues/852): on Mach-O, `.subsections_via_symbols` forbids
non-private labels between `.cfi_startproc`/`.cfi_endproc`, and newer LLVM began rejecting the CFI
math that the old hand-written aarch64 assembly relied on. It reproduces in vcpkg, Conan, and
MacPorts too — it is a libffi-source problem, not a mutsu one.

The CLAUDE.md note ("macOS is best-effort pending an upstream libffi Mach-O CFI fix") recorded this
as a *wait-for-upstream* item. This ADR re-evaluates now that upstream has shipped the fix.

## 2. What changed upstream (the fix now exists)

- **libffi itself** fixed the Mach-O CFI assembly in the **3.5.0** release line (subsequent
  point releases 3.5.x/3.6.0 carry it).
- **The `libffi-rs` project moved to a new maintainer org** (`libffi-rs/libffi-rs`; the old
  `tov/libffi-rs` is deprecated) and is actively released. Relevant `libffi-sys` history:
  - `4.0.0` (2025-10-15) — **bump vendored libffi to 3.5.2** (carries the Mach-O CFI fix); also
    corrected the `ffi_closure` layout for macOS aarch64. Breaking changes are confined to the
    low-level surface (`FFI_TYPE_*` became `u16`), which mutsu does not touch.
  - `4.2.0` (2026-06-21) — **bump vendored libffi to 3.6.0**.
- High-level crate mapping: `libffi` 5.1.1 → `libffi-sys` 4.2.0 → vendored libffi 3.6.0. MSRV 1.78
  (mutsu is on 1.93).

mutsu uses **only the `middle` API** (`Cif`, `Type`, `Arg`, `CodePtr`, `arg`) — verified in
`src/runtime/nativecall.rs`. That surface is stable across libffi 3→5; no `Closure`/`low`/`raw`
usage, so the 4.0.0 breaking changes do not reach us. The only source change required was one
elided-lifetime annotation (`Arg` → `Arg<'_>`), which a new rustc lint flagged.

## 3. Options considered

### Option A — bump the vendored build (`libffi = "5"`) — CHOSEN

Keep libffi-sys's default in-tree C build; just move to a version whose vendored source already
contains the Mach-O CFI fix.

- **+** Hermetic: no build-time system dependency. The libffi C source is compiled from the crate,
  exactly as today, on every target (Linux x64/arm64, macOS x64/arm64) and for anyone building mutsu
  from source with `cargo build`. Nothing new needs to be installed on CI runners or user machines.
- **+** Minimal, reversible diff: a version bump plus one lifetime annotation. API-compatible.
- **+** Fixes the actual reported failure (the vendored source was the broken artifact).
- **+** Stays on the maintained crate line (`libffi-rs/libffi-rs`), so future toolchain breakage gets
  picked up by a routine version bump.
- **−** Still building C at compile time (needs a C compiler in the build environment) — but that was
  already true and is satisfied on all our runners.

### Option B — `libffi-sys` `system` feature (link the OS-provided libffi)

Enable `libffi-sys/system` so the crate links a preinstalled system libffi instead of building its
own.

- **+** Sidesteps the vendored-assembly problem by not compiling libffi at all.
- **−** **Loses build hermeticity.** Every build machine — CI runners *and* any user running
  `cargo build` — must have a sufficiently recent libffi (≥ 3.2.1) plus `pkg-config` present, or the
  build fails to *find* libffi instead of failing to *compile* it. This trades a fixed, already-solved
  compile error for an open-ended environment requirement, and it would newly burden the Linux path
  that currently Just Works.
- **−** Runtime/ABI coupling to whatever libffi the host ships; version skew becomes a support
  surface.
- **−** Larger, more invasive change (feature wiring, per-platform install steps in release.yml and
  docker.yml) for a strictly worse hermeticity story.
- Verdict: appropriate only as a fallback if a future toolchain breaks the vendored build again *and*
  no fixed vendored release exists yet. Not warranted now.

### Option C — status quo (keep `continue-on-error`, ship no macOS binaries)

Rejected: the fix now exists upstream, and silently shipping no macOS binaries is the exact failure
this ADR exists to end.

## 4. Decision

- **Adopt Option A: bump `libffi` from `"3"` to `"5"`** (→ libffi-sys 4.2.0, vendored libffi 3.6.0).
  Apply the one required `Arg<'_>` lifetime annotation in `nativecall.rs`.
- **Keep the in-tree vendored build.** Do **not** enable the `system` feature; hermeticity across all
  targets is worth more than avoiding a now-fixed compile error.
- **Verify the macOS arm64 build end-to-end via `workflow_dispatch`** on the branch before trusting
  it (the release workflow already supports a tag-less smoke run). Only once a real macOS runner
  builds green do we tighten the safety net.
- **Re-tighten `continue-on-error` for macOS** once verified, so a future macOS regression fails
  loudly instead of silently dropping binaries again. (If the verification run is red for an
  unrelated reason, leave `continue-on-error` and record the residual blocker rather than blocking
  the Linux release.)

## 5. Consequences

- `Cargo.toml`/`Cargo.lock`: `libffi` 5.1.1, `libffi-sys` 4.2.0. All NativeCall `t/nativecall-*.t`
  tests (including the real CIF/call paths in `nativecall-sqlite.t`, `nativecall-pointer.t`,
  `nativecall-mvp.t`) pass on Linux post-bump; `cargo clippy -- -D warnings` is clean.
- `release.yml` and the CLAUDE.md "Distribution" note are updated: the fix is no longer "pending
  upstream"; it is vendored in. macOS arm64 is expected to build.
- Future toolchain breakage of the vendored assembly should first be answered by bumping the crate
  (a fixed vendored release usually lands quickly), reserving Option B for the case where no fixed
  vendored release exists.
- **Follow-up (tracked here, not yet done in this PR unless the smoke run is green):** after a green
  `workflow_dispatch` macOS run, remove `continue-on-error`/`optional` for `aarch64-apple-darwin`
  (and `x86_64-apple-darwin`) so the release fails loudly on a macOS regression.
