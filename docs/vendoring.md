# Vendored upstream sources

Three directories in this repository are copied verbatim from upstream Raku
projects rather than written here:

| Directory         | Upstream                                   | What it is                                    |
| ----------------- | ------------------------------------------ | --------------------------------------------- |
| `roast/`          | <https://github.com/Raku/roast>            | The official Raku spec test suite             |
| `raku-doc/`       | <https://github.com/Raku/doc>              | The official Raku language/type documentation |
| `old-design-docs/`| <https://github.com/Raku/old-design-docs>  | The historical Perl 6 design synopses         |

## Why vendored, not git submodules

These are deliberately **vendored** (a plain copy committed into this repo), not
git submodules. With a submodule, the upstream repo is checked out *with its
`.git` and remote intact* underneath this workspace, which makes it far too easy
to accidentally `git push` a branch, open a pull request, or file an issue
against the upstream **Raku org** repositories from here. Project policy forbids
that (see `CLAUDE.md`: "Do not create PRs or Issues against Raku org repositories
… from this workspace"). A vendored copy has no upstream remote, so the mistake
is impossible.

The trees are **read-only**: never hand-edit files under `roast/`, `raku-doc/`,
or `old-design-docs/`. The only supported way to change them is to re-vendor a
newer upstream commit with the tool below.

## Version tracking: `vendor.lock`

`vendor.lock` (repo root) records, for each tree, the upstream URL, the tracked
branch, and the **exact commit** the current copy was imported from, plus that
commit's date. This makes the vendored version auditable and reproducible — you
can always tell precisely which upstream revision the tests/docs correspond to.

Do not edit the data rows by hand; the tool below rewrites them.

## Updating: `scripts/update-vendor.sh`

```sh
# See how far behind upstream each tree is (no changes made):
scripts/update-vendor.sh --check

# Re-vendor one tree at its tracked branch HEAD, updating vendor.lock:
scripts/update-vendor.sh raku-doc

# Re-vendor at a specific commit/tag instead of branch HEAD:
scripts/update-vendor.sh roast b2cbe8a42eaf9a044dc95f544428bffdeb2870c7

# Re-vendor every tree at its branch HEAD:
scripts/update-vendor.sh --all
```

The tool clones the upstream repo (blob-filtered, so it is fast), checks out the
requested ref, `rsync --delete`s the tree into place (making it an exact copy of
upstream at that commit — `.git` and local build artifacts such as
`roast/.precomp` are excluded/removed), and rewrites the tree's row in
`vendor.lock` with the new commit and date.

After running it, review the diff with `git status <dir>/ vendor.lock` and
commit.

### Updating `roast` has extra follow-up

`roast-whitelist.txt` lists roast tests that pass **completely**. When you
re-vendor a newer `roast`, upstream may have added subtests to or changed the
expectations of files that are currently whitelisted. After a roast update:

1. Re-run every whitelisted file that upstream changed
   (`scripts/update-vendor.sh --check` tells you roast moved; compare
   `git diff --name-only` in the update against `roast-whitelist.txt`).
2. Any file that no longer passes must be removed from `roast-whitelist.txt` and
   recorded in `TODO_roast/BLOCKERS.md` with the reason.
3. New spec features documented in the update may be worth adding to `PLAN.md`.

Because of this test-suite churn, roast updates are done in their own PR,
separate from the doc-only trees.
