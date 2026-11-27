---
name: install-raku
description: Install an upstream Rakudo prebuilt binary so that `raku` is available as mutsu's reference oracle. Use when `raku: command not found` in a fresh or ephemeral container blocks comparing mutsu against Rakudo, measuring `.AST` shapes, or running a roast test under raku.
metadata:
  short-description: Make `raku` available from a rakudo.org prebuilt
---

# Install a Rakudo prebuilt binary

mutsu development depends on a working `raku` as an oracle: `raku -e '<code>'` decides
expected behaviour when the spec is unclear, `raku <roast-test>` shows the expected output
before mutsu is compared against it, and every RakuAST slice starts by measuring the Rakudo
`.AST` shape (`docs/rakuast/README.md`). A fresh remote or ephemeral container has no rakudo,
and Ubuntu's `apt` package is 2022.12 — far too old for RakuAST. Do not work without the
oracle; install the prebuilt instead.

## Install

```bash
.agents/skills/install-raku/install-raku.sh
```

It takes a few seconds and needs no compiler: the rakudo.org archive is a self-contained,
relocatable tree. It installs the release pinned in **`.rakudo-version`** at the repository
root — the one rakudo release the oracle, the bench CI's ratio baseline (`bench.yml`) and the
ecosystem sweep all use, so an expected output checked here is the rakudo the numbers are
measured against. The script is idempotent: if `raku` of that release is already on `PATH` it
reports the version and exits 0, so it is safe to run at the start of any session. A `raku` of
another release is not replaced in place; the pinned one is installed next to it and symlinked
into the bindir.

What it does:

1. Fetches the JSON release index from `https://rakudo.org/dl/rakudo`.
2. Selects the entries with `type: archive`, `backend: moar`, this machine's
   `platform`/`arch` and the pinned `ver`, and takes the highest `build_rev` (with `--latest`:
   the newest entry, ordered by `(ver, build_rev)`).
3. Downloads the tarball and verifies its SHA256 against the published `.checksums.txt`.
4. Unpacks it to `~/.local/rakudo/<release>/` and symlinks `bin/*` into `~/.local/bin/`.
5. Runs `raku -e 'say $*RAKU.compiler.version'` to prove the install works, and warns if the
   bindir is not on `PATH`.

**Do not select on the index's `latest` flag.** It marks the newest *source* release, which is
routinely published before that release's binary builds exist — on 2026-09-05 `latest: 1` was
2026.08 (src only) while the newest linux prebuilt was 2026.07. Ordering by `(ver, build_rev)`
over the filtered archive entries is the correct selection.

## Options

| Flag | Effect |
| --- | --- |
| `--prefix DIR` | Unpack location (default `~/.local/rakudo`, or `$RAKUDO_PREFIX`) |
| `--bindir DIR` | Symlink location (default `~/.local/bin`, or `$RAKUDO_BINDIR`) |
| `--version VER` | Install another release than the pin, e.g. `--version 2026.07` |
| `--latest` | Install the newest prebuilt instead of the pin |
| `--force` | Reinstall even when the wanted `raku` is already on `PATH` |
| `--no-verify` | Skip the SHA256 check |
| `--print-url` | Print the selected tarball URL and exit (useful for a dry run) |
| `--print-pin` | Print the release in `.rakudo-version` and exit |

## Moving to a new Rakudo release

Edit `.rakudo-version` (one line, e.g. `2026.11`) and nothing else: `bench.yml`, the ecosystem
sweep and the session-start hook all read it through this script, and `make check-rakudo-pin`
fails on a workflow, hook or script that names a release of its own. A bump steps every bench
ratio at once; bench-trend marks the change on its ratio charts. Re-measure before raising
`RAKUDO_COMPAT_VERSION` (ADR-0104): it is a compatibility claim, not an install pin.

## Platform limits

Prebuilts exist for `linux/x86_64`, `macos/x86_64`, `macos/arm64` and `win/x86_64` only.
There is **no `linux/arm64` prebuilt**; the script fails there with a clear message. On such a
host either build from source with [rakubrew](https://rakubrew.org/) (slow, needs a toolchain)
or run the oracle inside the official `rakudo/rakudo` container image.

## Confirm the oracle works

```bash
raku --version
raku -e 'say (1..5).map(* ** 2)'          # behaviour oracle
raku -e 'say Q|say 42|.AST'               # RakuAST shape oracle
raku roast/S02-literals/numeric.t | head  # expected roast output
```

`--target=ast` dumps QAST, not RakuAST — use `Q|...|.AST` for the node shapes that RakuAST
slices are measured against.

## Scope

- The archive ships `raku`, `rakudo`, `nqp`, `moar` and the `perl6` aliases. It does **not**
  ship `zef`; mutsu vendors its own copy under `vendor/zef/`, so nothing here needs it.
- This installs a reference implementation for comparison only. It is not part of mutsu's
  build or test pipeline, and nothing under `~/.local/rakudo` belongs in the repository.
