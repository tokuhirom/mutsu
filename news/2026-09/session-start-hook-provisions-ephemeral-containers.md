# Ephemeral containers now provision their own rustc and raku

Sessions launched from the Claude app (Claude Code on the web) run in a container that is created
fresh from a base image, and that image is not this repository's environment. Two things were
missing every single time:

- **rustc was behind the code.** The image shipped 1.94.1 while the repo builds with 1.96.x, so the
  first `cargo build` died with `E0658` pointing at a line in `src/` — a failure that looks like a
  code error and invites "fixing" the source to suit an old compiler. `.claude/skills/rustc-too-old/`
  exists precisely because that misdiagnosis kept happening.
- **`raku` was absent.** Any work that has to *measure* Rakudo's behaviour (RakuAST node shapes above
  all) is blocked without the oracle, and Ubuntu's packaged rakudo is far too old to serve as one.

Both fixes were already written down — the `rustc-too-old` and `install-raku` skills — but a skill is
only read when an agent already suspects the problem, which is after it has wasted the session on the
symptom. The knowledge was in the wrong shape: this is provisioning, not diagnosis.

`.claude/hooks/session-start.sh`, registered as a `SessionStart` hook in `.claude/settings.json`, now
runs the provisioning before the agent's first command:

1. **Rust.** Three places in the repo declare a Rust version and they drift apart — the `ci.yml`
   `dtolnay/rust-toolchain` pin comment (authoritative: CI demonstrably builds with it),
   `Cargo.toml`'s `rust-version` MSRV, and `.mise.toml`'s local pin. The hook reads all three and
   takes the highest via `sort -V`, so it tracks whichever one moves without ever needing an edit
   itself. When the installed compiler is older it runs `rustup toolchain install <ver> --profile
   minimal -c clippy -c rustfmt` (both components are needed by the pre-commit hooks and CI, and
   `minimal` omits them) and makes it the default.
2. **Raku.** Delegates to the existing `.agents/skills/install-raku/install-raku.sh`, which picks the
   newest `moar`/`archive` prebuilt for the platform out of the rakudo.org index, verifies its
   SHA256, and symlinks `bin/*` into `~/.local/bin`.
3. **Crate cache.** `cargo fetch`, so the first build is compile-only. The container image is
   snapshotted after the hook, so the download is paid once per image rather than once per session.

`~/.local/bin` and `~/.cargo/bin` are appended to `PATH` through `$CLAUDE_ENV_FILE` so the session
inherits them.

The hook is **synchronous** on purpose: async startup would let the agent begin work — and quite
possibly start a build against the stale compiler — while the toolchain was still installing, which
is exactly the race the hook exists to remove. Its `timeout` is set to 900s in `settings.json`
because a cold rustup + rakudo download takes minutes, well past the 60s hook default. It is
idempotent and costs ~0.3s once the container is warm.

It also does nothing on a local checkout (it exits unless `CLAUDE_CODE_REMOTE=true`, overridable with
`MUTSU_SETUP_FORCE=1`): a developer machine is pinned by `.mise.toml` and owns its own toolchain, and
silently repointing someone's `rustup default` from a repo file would be a rude surprise.

Verified in a live remote container: 1.94.1 → 1.96.1, Rakudo v2026.07 installed from scratch, a
second run reporting "rustc is new enough / raku already present" in 0.26s.
