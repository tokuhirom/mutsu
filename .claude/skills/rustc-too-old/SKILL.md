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

The tell is an error about the *language*, not about your program's meaning:

```
error[E0658]: `if let` guards are experimental
   --> src/vm/vm_data_ops.rs:134:19
    = note: see issue #51114 <https://github.com/rust-lang/rust/issues/51114> for more information
    = help: you can write `if matches!(<expr>, <pattern>)` instead of `if let <pattern> = <expr>`
```

`E0658` is the canonical one, but the family also includes "is unstable", "use
of unstable library feature", and "requires nightly". A `= note: see issue
NNNNN` line pointing at a rust-lang tracking issue is a strong signal.

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

`Cargo.toml`'s `rust-version` is **not** that answer, and trusting it will send
you in circles. It is a declared MSRV that cargo checks against the running
toolchain — it does not track which features the source has since started using.
At the time of writing it says `1.94.0` while the code needs `1.96.0`, so an
installed 1.94.1 satisfies cargo's gate and then fails to compile. If you notice
that gap, it is a real (if harmless) inconsistency worth mentioning to the user;
it is not something to "fix" by editing either number to make an error go away.

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
- **Do not edit `Cargo.toml`'s `rust-version`** to silence anything. It gates
  nothing you are hitting.
