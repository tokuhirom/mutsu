# mutsu 1.0.0

mutsu 1.0.0 was released on 2026-11-27. It is the first release under the monthly cadence and the
release gate in PLAN.md.

At release time:

- **Ecosystem:** every distribution in the zef ecosystem that can be graded against a rakudo
  baseline (1,338) passed every test file that rakudo 2026.11 passes. Assertion parity was 100%.
  Out-of-scope distributions and accepted divergences are listed in `ecosystem/`.
- **Roast:** 1440 of 1455 files are whitelisted. Each remaining file is listed with its reason in
  `docs/roast-compatibility.md`.
- **Release pipeline:** a smoke run before the tag found three release-blocking problems, all fixed
  first:
  - `release.yml` could not call `ci.yml` with its read-only grant (#19190);
  - the macOS targets did not compile (#19191);
  - the Docker build context missed `linker/` (#19209).
- **mzef:** installing a dist, running its tests, using it and uninstalling it now work end to
  end, and an offline test in CI covers that path (#19252-#19255).

The release ships four tarballs (Linux and macOS, x64 and arm64) with build provenance, the
`@tokuhirom/mutsu` npm package, and the `ghcr.io/tokuhirom/mutsu:1.0.0` image.
