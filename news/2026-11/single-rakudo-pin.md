# One rakudo pin for the oracle, the bench baseline and the ecosystem sweep

Three places installed rakudo, and each chose the release on its own. `bench.yml` hard-coded a
rakudo.org archive name. The agents' oracle (`install-raku.sh`, run by the session-start hook)
and the ecosystem sweep took "the newest prebuilt". They drifted apart: the bench measured its
ratios against Rakudo 2026.07 well after the oracle had moved on, until #19269 bumped it by
hand.

The release now lives in one file, `.rakudo-version` (currently `2026.11`), and everything reads
it through `install-raku.sh`:

- `install-raku.sh` installs the pinned release by default (`--latest` for the newest,
  `--version VER` for another) and prints it with `--print-pin`. A `raku` of another release on
  `PATH` no longer counts as installed: the pinned one goes next to it.
- The session-start hook checks the installed `raku` against the pin, not only for presence.
- `bench.yml` installs the pin through the script, which also verifies the archive's SHA256
  against rakudo.org's checksum file. Before, it piped the download straight into `tar`.
- The ecosystem sweep's empty `raku_version` input now means the pin, not the newest prebuilt.

`make check-rakudo-pin`, also run in CI's always-on `changes` job, fails when `.rakudo-version` is
not one release, or when a workflow, hook, skill or script names a rakudo.org archive or passes
a literal `--version` of its own. Nothing changes version here: every consumer was already on
2026.11 or was going to resolve to it.
