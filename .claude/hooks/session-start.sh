#!/usr/bin/env bash
# SessionStart hook: bring a freshly provisioned container up to the toolchain
# this repository needs, before the agent runs its first command.
#
# Two things are routinely missing or stale in the ephemeral containers that
# Claude Code on the web / the Claude app provision:
#
#   1. rustc — the base image ships whatever stable it was built with, which is
#      regularly older than the version this repo compiles under. The failure
#      mode is a confusing E0658 pointing at a line in src/ (see
#      .claude/skills/rustc-too-old/SKILL.md).
#   2. raku — the Rakudo oracle used to check expected behaviour is absent
#      entirely, and Ubuntu's packaged one is far too old to be useful.
#
# Both are mechanical to fix, so fix them here rather than paying for the
# rediscovery every session.
#
# Local checkouts are left alone: a developer machine is pinned by .mise.toml
# and owns its own toolchain. Set MUTSU_SETUP_FORCE=1 to run it anyway.
set -euo pipefail

if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ] && [ "${MUTSU_SETUP_FORCE:-}" != "1" ]; then
  exit 0
fi

REPO="${CLAUDE_PROJECT_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
cd "$REPO"

say() { printf 'session-start: %s\n' "$*"; }
warn() { printf 'session-start: WARNING: %s\n' "$*" >&2; }

# ------------------------------------------------------------------- rustc --
# Three places declare a Rust version and they can drift apart, so take the
# highest: the CI toolchain pin (authoritative — CI demonstrably builds with
# it), the MSRV in Cargo.toml (cargo refuses outright below it), and the
# .mise.toml pin developers use locally.
wanted_rust() {
  {
    sed -n 's|.*dtolnay/rust-toolchain@[0-9a-f]\{6,\} *# *\([0-9][0-9.]*\).*|\1|p' \
      .github/workflows/ci.yml
    sed -n 's/^rust-version *= *"\([0-9][0-9.]*\)".*/\1/p' Cargo.toml
    sed -n 's/^rust *= *"\([0-9][0-9.]*\)".*/\1/p' .mise.toml
  } 2>/dev/null | sort -V | tail -n1
}

setup_rust() {
  local want have
  want="$(wanted_rust || true)"
  if [ -z "$want" ]; then
    warn "could not determine the Rust version this repo wants; leaving the toolchain alone"
    return 0
  fi
  have="$(rustc --version 2>/dev/null | awk '{print $2}')"
  say "rustc: have ${have:-none}, repo wants $want"

  # sort -V puts the smaller first; if `want` is already the smaller of the
  # two, the installed compiler is new enough.
  if [ -n "$have" ] && [ "$(printf '%s\n%s\n' "$want" "$have" | sort -V | head -n1)" = "$want" ]; then
    say "rustc is new enough"
    return 0
  fi

  if ! command -v rustup >/dev/null 2>&1; then
    warn "rustc $want is needed but rustup is not installed; see https://rustup.rs"
    return 0
  fi

  # clippy and rustfmt are not in --profile minimal, and both the pre-commit
  # hooks and CI run them, so install them up front.
  say "installing rustc $want via rustup (a few minutes on a cold container)"
  if rustup toolchain install "$want" --profile minimal -c clippy -c rustfmt \
     && rustup default "$want"; then
    say "rustc is now $(rustc --version | awk '{print $2}')"
  else
    warn "rustup could not install $want; the build may fail with E0658 (see .claude/skills/rustc-too-old/SKILL.md)"
  fi
}

# -------------------------------------------------------------------- raku --
# The Rakudo oracle. install-raku.sh is a no-op when a working `raku` is
# already on PATH, so this is safe to re-run.
setup_raku() {
  export PATH="$HOME/.local/bin:$PATH"
  if command -v raku >/dev/null 2>&1; then
    say "raku already present: $(command -v raku)"
    return 0
  fi
  say "installing the Rakudo oracle (prebuilt archive, no compilation)"
  if ! .agents/skills/install-raku/install-raku.sh; then
    warn "could not install raku; work that needs the oracle will have to do without it"
    return 0
  fi
  say "raku installed: $(raku --version 2>/dev/null | head -n1)"
}

setup_rust
setup_raku

# Warm the crate cache so the first build is compile-only. Cheap next to the
# build itself, and the container image is snapshotted after this hook.
cargo fetch --locked >/dev/null 2>&1 || cargo fetch >/dev/null 2>&1 || \
  warn "cargo fetch failed; the first build will download crates itself"

# Persist PATH for the session: ~/.local/bin holds the raku symlinks,
# ~/.cargo/bin the rustup shims.
if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
  printf 'export PATH="%s/.local/bin:%s/.cargo/bin:$PATH"\n' "$HOME" "$HOME" >> "$CLAUDE_ENV_FILE"
fi

say "environment ready"
