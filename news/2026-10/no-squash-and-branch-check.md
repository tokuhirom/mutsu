# Never squash a branch, and the gate now checks the branch's diff

#10983 reverted three merged PRs (restored by #11005). The cause was a squash:
its branch was squashed with `git reset --soft origin/main` right after a
fetch. By then `origin/main` had moved past the branch's base, so the single
commit put the branch's *old* tree on top of the newer `main`, and the merge
silently undid everything merged in between.

Two changes keep that from happening again.

- **AGENTS.md now forbids squashing or otherwise rewriting a branch's
  commits.** Containers are disposable, so work is committed and pushed as it
  goes, and those commits are the branch's history. The repository merges with
  merge commits, so a branch of small commits costs nothing. Each commit now
  gets a real message instead of a bare `wip`. The only rewrite left is
  `git rebase origin/main` to resolve a conflict, which replays the branch's
  own commits.
- **`scripts/dev gate` starts with a new `branch` stage**
  (`scripts/dev branch-check`). It lists every file the branch changes
  against its merge base with `origin/main`, so the list can be read against
  what the change meant to touch. It fails when one of those files is back at
  an *older* `main` state rather than the merge base's: the exact shape of a
  stale tree committed on a newer `main`. Run against the commit that #10983
  merged, it flags all 19 files the merge reverted and none of the six the PR
  meant to change. It reports no files on the session's other RakuAST
  branches. A branch that carries a `git revert` is exempt, since older states
  are what a revert produces. A self-test rebuilds the failure in a scratch
  repository.
