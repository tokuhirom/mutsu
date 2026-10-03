# Releases no longer need a GitHub App

Cutting a release used to mean dispatching `tag-release.yml`. That workflow minted a token from a
GitHub App which was a bypass actor on the `main` ruleset. It used the token for two things:

- committing the version bump straight to `main`, skipping review and CI;
- pushing the `vX.Y.Z` tag. A tag pushed with the default `GITHUB_TOKEN` does not start
  `release.yml`, so the workflow needed the App token for this too.

A release is now two ordinary steps:

1. **A version-bump pull request** (`chore(release): vX.Y.Z`). It goes through full CI and
   auto-merge like every other change, so the commit that gets tagged has passed CI.
2. **A tag on the bump PR's merge commit.** The maintainer's account pushes it, either at a
   terminal or from a session's `git push`.

`release.yml` gains a `verify-tag` job that runs before anything builds or publishes. It refuses
a tag whose version differs from the tagged commit's `Cargo.toml`, and a tag whose commit is not
on `main`, so a mistyped or stray tag publishes nothing. The `cut-release` skill describes the
new procedure. AGENTS.md now says a `v*` tag is pushed only by that skill, and only when the user
asked for a release.

With the ecosystem sweep already landed by a Claude Code routine, no workflow holds an App key or
a ruleset-bypass token any more. The App and its `TAGPR_APP_*` credentials can be removed.
