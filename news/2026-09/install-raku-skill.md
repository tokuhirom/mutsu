# `install-raku` skill: get the Rakudo oracle back in one command

Almost every mutsu workflow leans on a working `raku` as the reference oracle —
`raku -e '<code>'` decides expected behaviour when the spec is unclear, `raku
<roast-test>` shows the expected output before mutsu is compared against it, and
every RakuAST slice starts by measuring the Rakudo `.AST` shape. A fresh remote
or ephemeral container has no rakudo at all, and Ubuntu's `apt` package is
2022.12, far too old for RakuAST. Until now CLAUDE.md carried a prose recipe for
recovering from that (fetch the rakudo.org JSON index, pick an entry, untar,
symlink), which had to be re-derived by hand in every new container.

That recipe is now a skill: `.agents/skills/install-raku/`, with an executable
`install-raku.sh`. It fetches `https://rakudo.org/dl/rakudo`, selects the newest
`type: archive` / `backend: moar` entry for the host platform and arch,
downloads the tarball, verifies its SHA256 against the published
`.checksums.txt`, unpacks it to `~/.local/rakudo/<release>/`, symlinks `bin/*`
into `~/.local/bin/`, and proves the result by running
`raku -e 'say $*RAKU.compiler.version'`. On this container the whole run takes
about five seconds. It is idempotent: when a working `raku` is already on `PATH`
it prints the version and exits 0, so it is safe to run unconditionally at the
start of a session. `--prefix`, `--bindir`, `--version`, `--force`,
`--no-verify` and `--print-url` cover the remaining cases; release selection
works through either `python3` or `jq`, whichever the host has.

Two details are worth recording because both are easy to get wrong by hand.
First, **the index's `latest` flag must not be used to select the download**: it
marks the newest *source* release, which is routinely published before that
release's binary builds exist. On 2026-09-05 `latest: 1` was 2026.08 (src only)
while the newest linux prebuilt was 2026.07, so a `latest`-driven selection
finds nothing installable. Ordering by `(ver, build_rev)` across the filtered
archive entries is the correct rule. Second, **rakudo.org publishes no
`linux/arm64` prebuilt** — only `linux/x86_64`, `macos/x86_64`, `macos/arm64`
and `win/x86_64`. The script detects that case and fails with a message pointing
at rakubrew or the official container image, rather than downloading a tarball
for the wrong architecture.

The prebuilt tree ships `raku`, `rakudo`, `nqp`, `moar` and the `perl6` aliases,
but not `zef`; mutsu vendors its own copy under `vendor/zef/`, so nothing here
depends on that. CLAUDE.md's "Reference implementation" section now points at
the skill instead of restating the manual steps.
