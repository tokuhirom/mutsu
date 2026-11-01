# The Docker build context carries `crates/`

`docker.yml` failed on the v0.24.0 and v0.26.0 tags: `cargo build` in the
builder stage could not load the workspace member `crates/mutsu-lsp`, because
the allowlist in `.dockerignore` did not re-include `crates/`. The allowlist now
has `!crates/`, so the next tag publishes `ghcr.io/tokuhirom/mutsu`.
`docker.yml` runs only on a tag, so no pull request exercises this path.
