# Fix the Docker release build: allow modules/ into the build context

The Docker image build (`docker.yml`, GHCR) had been failing on every tag since
v0.17.0 with:

```
ERROR: failed to compute cache key: failed to calculate checksum of ref ...:
"/src/modules": not found
```

`.dockerignore` uses an ignore-everything-then-allowlist strategy (`*` followed
by `!Cargo.toml`, `!src/`, `!vendor/`, …) to keep the build context tiny. When
the OpenSSL / IO::Socket::SSL battery landed (#5342), it added the `modules/`
directory and a `COPY --from=builder /src/modules …` line to the `Dockerfile`,
but did not add `!modules/` to the allowlist. So `modules/` was excluded from the
build context, `COPY . .` never copied it into the builder, and the runtime-stage
`COPY /src/modules` failed.

The tarball release (`release.yml`) was unaffected — it copies `modules/`
straight from the checkout, not through the Docker context — so v0.17.0 and
v0.17.1 shipped tarballs but no container image. Adding `!modules/` to
`.dockerignore` restores the Docker publish.
