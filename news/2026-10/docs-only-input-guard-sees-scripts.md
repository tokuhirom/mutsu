# The docs-only input guard actually runs in CI now

`scripts/ci-docs-only.sh --check-inputs` exists to stop a build input from
being classified as documentation. A docs-only PR skips the build jobs, so an
allowlisted input could change without anything testing it. The guard reads
every repository path named in the Makefile and in `ci.yml`, and fails if one of
those paths is on the allowlist. It drops paths that do not exist, because the
scan also picks up runner paths.

In CI it ran inside the `changes` job's sparse checkout, which contained only the
classifier itself. Every `scripts/*.py` input therefore "did not exist" and was
dropped, so the guard checked nothing. Two ratchet scripts run by `make checks`,
`scripts/check-ast-walkers.py` and `scripts/check-interp-construction.py`, sat
on the `scripts/*.py` allowlist. A PR that only edited one of them, for example
to loosen the ratchet, was classified docs-only and skipped `test-check`.

- Both scripts are now denied in `is_doc_path`, with self-test cases.
- The `changes` job checks out all of `scripts/`.
- `--check-inputs` treats a missing `scripts/` path as an error. Every one of
  those paths is a real file, so a missing one means the checkout is
  incomplete. A sparse checkout that is too narrow now fails loudly instead of
  turning the guard into a no-op.
