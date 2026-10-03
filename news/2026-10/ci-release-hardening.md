# CI and release workflows hardened

The workflows now follow the "CI, release and agents" rules in `docs/security.md`.
`zizmor --offline .github/workflows` went from 20 high and 34 medium findings to none.
The few that remain intentional are ignored with a comment that gives the reason.

- **App token and npm publishing.** The release App token (a ruleset bypass actor) and npm
  publishing now run only in jobs bound to a `release` environment. Only `main` and `v*` tags may
  deploy to that environment. The token is also scoped to the permissions each job needs, instead
  of the App's whole installation.
- **Workflow inputs.** `tag-release.yml` passes the version input to its shell steps through the
  environment instead of expanding `${{ inputs.version }}` into the script text.
- **Token permissions.** Every workflow declares read-only top-level `permissions:`, and only the
  jobs that write raise them. Checkouts that do not push no longer persist the token.
- **wasm-pack.** It is installed from a pinned, SHA-256-checked release tarball instead of
  `curl … | sh` from the archived rustwasm site.
- **Release artifacts.** Release tarballs carry signed build-provenance attestations. The Pages
  deploy runs `npm audit signatures` on the package it is about to serve.
- **Ecosystem sweep sandbox.** The sweep shards run third-party test suites and now hold only a
  read-only token. `sandbox_wrap` unsets secret-looking environment variables, including the
  runner's `ACTIONS_RUNTIME_TOKEN`, and masks credential files such as `~/.ssh`, `~/.config/gh` and
  the repository's `.env`.
- **Claim log.** The claim-log sync ignores `Claiming:` comments from accounts without write access,
  and claims that name a long-lived branch such as `main`.
- **Code owners.** `.github/CODEOWNERS` marks the CI, release and agent-configuration paths for
  maintainer review. AGENTS.md now says an agent never merges a PR itself and never bypasses the
  ruleset: it acts under the maintainer's admin account, so auto-merge is the only way its PRs land.
- **`.env` files.** `.env` files are ignored.
