---
name: rustc-too-old
description: Diagnose and fix a rustc/toolchain that is older than the code needs, in a container where `cargo build` fails on a language feature rather than on the code. Use this whenever a build dies with E0658, "are experimental", "unstable", "requires nightly", "add `#![feature(...)]`", "consider using an edition", or an unexplained "could not compile `mutsu` (lib)" — and also whenever the session is on a fresh or ephemeral machine and you are about to build for the first time, or the user asks how to upgrade rustc, which Rust version this repo wants, or why a build works in CI but not here. Reach for it before you consider rewriting the code to suit the compiler.
---

# The compiler is behind the code, not the other way round

A remote or newly-provisioned container often ships whatever rustc the base
image happened to have. This repo tracks a recent stable and uses features that
land in it, so the first `cargo build` on such a machine can fail on syntax the
code has used for months. Nothing is wrong with the code — the toolchain is the
thing to move.

The failure mode worth naming: this looks like a code error. It cites a file and
a line in `src/`, so the reflex is to "fix" that line. Don't. Check the compiler
version first; it costs one command.

## 1. Recognize it

There are two shapes, and which one you get depends on whether the declared
MSRV is currently accurate.

**The good one** — cargo refuses before compiling anything, because
`Cargo.toml`'s `rust-version` is higher than the installed toolchain:

```
error: rustc 1.94.1 is not supported by the following package:
  mutsu@0.23.0 requires rustc 1.96.0
```

That is unambiguous: go to step 2. Note that cargo checks *every* package in
the workspace, so this fires even for `cargo check -p mutsu-lsp`, which does
not build the interpreter at all.

**The confusing one** — the declaration has drifted behind what the source
actually uses, so cargo's gate passes and the compiler then rejects the
*language*, not your program's meaning:

```
error[E0658]: `if let` guards are experimental
   --> src/vm/vm_data_ops.rs:134:19
    = note: see issue #51114 <https://github.com/rust-lang/rust/issues/51114> for more information
    = help: you can write `if matches!(<expr>, <pattern>)` instead of `if let <pattern> = <expr>`
```

`E0658` is the canonical one, but the family also includes "is unstable", "use
of unstable library feature", and "requires nightly". A `= note: see issue
NNNNN` line pointing at a rust-lang tracking issue is a strong signal.

Seeing this second shape means the MSRV declaration is stale — worth fixing
once you are unblocked, so the next person gets the first shape instead.

**Read the whole error, not the tail of the log.** A long build's last few lines
are just `error: could not compile ... due to 1 previous error`, which says
nothing. If you backgrounded the build or piped it through `tail`, re-read with
the error in view:

```bash
cargo build 2>&1 | grep -B2 -A 12 '^error' | head -60
```

Then confirm the suspicion:

```bash
rustc --version
```

## 2. Find the version the repo actually wants

**The authoritative source is the CI toolchain pin, not `Cargo.toml`.**

```bash
grep -n "rust-toolchain@" .github/workflows/ci.yml
```

The pinned action SHA carries a version comment:

```
- uses: dtolnay/rust-toolchain@01ba1edad32c6f80dbcce879d3e0fa5a00b2a84e # 1.96.0
```

That comment is the version CI builds with, so it is by definition a version the
code compiles under.

`Cargo.toml`'s `rust-version` is a useful first signal but not the
authoritative one. It is a hand-maintained MSRV declaration that cargo checks
against the running toolchain; it does not track which features the source has
since started using. It read `1.94.0` for a while after the code already needed
`1.96.0`, which is exactly how an installed 1.94.1 came to satisfy cargo's gate
and *then* fail to compile. Both manifests (root and `crates/mutsu-lsp/`) now
say `1.96.0`, so the two agree — but if they ever disagree again, the CI pin
wins, and the drift is worth reporting rather than papering over.

Note also that the repo has no `rust-toolchain.toml`, which is why the container
toolchain is whatever it is rather than being pinned per-checkout.

## 3. Install it and switch

```bash
rustup toolchain install 1.96.0 --profile minimal -c clippy -c rustfmt
rustup default 1.96.0
rustc --version   # confirm
```

Install `clippy` and `rustfmt` explicitly: `--profile minimal` omits them, and
this repo's pre-commit hooks and CI both run `cargo clippy -- -D warnings` and
`cargo fmt`. Discovering they are missing three commits later is a waste.

If `rustup` itself is absent (a distro-packaged rustc, no toolchain manager),
install rustup first from https://rustup.rs and then run the above.

## 4. Budget for the rebuild

Changing the default toolchain invalidates every previously built artifact, so
the next build is a full one — dependencies included. On a modest container
expect a few minutes for `cargo build` and closer to ten for
`cargo build --release`. Start it in the background and do something else rather
than watching it, but do not start unrelated CPU-heavy work alongside it on a
small box.

## What not to do

- **Do not rewrite the source to suit an old compiler.** Turning an `if let`
  guard into `matches!` because the local rustc is behind makes the diff about
  the container, not about the task, and CI would have accepted the original.
- **Do not switch to nightly** to get the feature. CI builds on pinned stable;
  a nightly-only local build hides real errors until the PR is open. (The one
  legitimate nightly here is the miri job, which pins its own dated nightly.)
- **Do not edit `Cargo.toml`'s `rust-version` to make an error go away.**
  Raising it when it has genuinely drifted behind the code is a real fix — it
  converts a future E0658 into cargo's legible refusal — but lowering it, or
  raising it in the hope that a compile error disappears, only misstates what
  the crate supports. It gates the toolchain, not the features.
