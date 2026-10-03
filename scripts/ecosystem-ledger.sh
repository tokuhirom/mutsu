#!/usr/bin/env bash
# ecosystem-ledger.sh -- move the ecosystem parity ledger between the
# `ecosystem-data` branch and the working tree.
#
# The ledger's *measurements* (ecosystem/dists/**.json, the rolled-up summary,
# history.tsv/.svg and the index snapshot) live on the orphan `ecosystem-data`
# branch, which .github/workflows/ecosystem-sweep.yml pushes to directly -- the
# same pattern as `bench-data` (ADR-0085, 2026-10-03 amendment). `main` keeps
# only the hand-maintained inputs: ecosystem/README.md, exclude.txt and
# accepted-divergences.toml. Every script keeps reading `ecosystem/...` as
# before; this script is what puts the measurements there (gitignored).
#
#   scripts/ecosystem-ledger.sh pull            # materialize the newest ledger
#   scripts/ecosystem-ledger.sh pull --ref REF  # ... or the one at REF
#   scripts/ecosystem-ledger.sh status          # which ledger commit is here
#   scripts/ecosystem-ledger.sh worktree DIR    # CI: a git checkout of the
#                                               # branch at DIR, then pull from it
#   scripts/ecosystem-ledger.sh stage DIR       # CI: copy ./ecosystem's
#                                               # measurements into DIR, so
#                                               # `git -C DIR status` is the diff
#
# `pull` replaces the local copy wholesale: a record the branch no longer has
# disappears here too. Local re-measurements (`ecosystem-sweep.py --only ...`)
# are therefore scratch data; the branch is written only by the sweep workflow.
set -euo pipefail

BRANCH=${MUTSU_ECOSYSTEM_DATA_BRANCH:-ecosystem-data}
REMOTE_REF="refs/remotes/origin/$BRANCH"
# Everything under ecosystem/ that a sweep writes. Keep in sync with .gitignore.
OUTPUTS=(dists summary.json summary.md history.tsv history.svg index-snapshot.json)
STAMP=ecosystem/.ledger-ref

REPO=$(git rev-parse --show-toplevel)
cd "$REPO"

die() { echo "ecosystem-ledger: $*" >&2; exit 1; }

fetch() {
  # A shallow fetch in CI, where every job starts from a fresh clone and only
  # the tip matters; a full one elsewhere, so a local repository never turns
  # shallow behind the user's back.
  local depth=()
  [ -n "${GITHUB_ACTIONS:-}" ] && depth=(--depth 1)
  git fetch -q "${depth[@]}" origin "+refs/heads/$BRANCH:$REMOTE_REF" \
    || die "could not fetch '$BRANCH' from origin (no network, or the branch does not exist yet)"
}

clear_outputs() {
  local root=$1 o
  for o in "${OUTPUTS[@]}"; do
    rm -rf "${root:?}/ecosystem/$o"
  done
}

# Copy the measurements of one ecosystem/ tree into another.
copy_outputs() {
  local from=$1 to=$2 o
  mkdir -p "$to/ecosystem"
  clear_outputs "$to"
  for o in "${OUTPUTS[@]}"; do
    if [ -e "$from/ecosystem/$o" ]; then
      cp -a "$from/ecosystem/$o" "$to/ecosystem/$o"
    fi
  done
}

cmd_pull() {
  local ref=""
  while [ $# -gt 0 ]; do
    case "$1" in
      --ref) ref=${2:?--ref needs a value}; shift 2 ;;
      *) die "pull: unknown argument '$1'" ;;
    esac
  done
  if [ -z "$ref" ]; then
    fetch
    ref=$REMOTE_REF
  fi
  local commit paths=() o
  commit=$(git rev-parse --verify -q "$ref^{commit}") || die "no such commit: $ref"
  for o in "${OUTPUTS[@]}"; do
    if git cat-file -e "$commit:ecosystem/$o" 2>/dev/null; then
      paths+=("ecosystem/$o")
    fi
  done
  [ ${#paths[@]} -gt 0 ] || die "$ref holds no ecosystem/ measurements"
  clear_outputs "$REPO"
  git archive --format=tar "$commit" -- "${paths[@]}" | tar -x -C "$REPO"
  echo "$commit" > "$STAMP"
  local n
  n=$(find ecosystem/dists -name '*.json' -type f 2>/dev/null | wc -l)
  echo "ecosystem-ledger: $n record(s) from $BRANCH @ $(git rev-parse --short "$commit")"
}

cmd_status() {
  [ -f "$STAMP" ] || { echo "no ledger here; run: scripts/ecosystem-ledger.sh pull"; return 1; }
  local here
  here=$(cat "$STAMP")
  echo "local ledger: $BRANCH @ $(git rev-parse --short "$here" 2>/dev/null || echo "$here")"
  if git rev-parse --verify -q "$REMOTE_REF" >/dev/null; then
    if [ "$(git rev-parse "$REMOTE_REF")" = "$here" ]; then
      echo "up to date with origin/$BRANCH (as last fetched)"
    else
      echo "origin/$BRANCH is at $(git rev-parse --short "$REMOTE_REF"); run: scripts/ecosystem-ledger.sh pull"
    fi
  fi
}

cmd_worktree() {
  local dir=${1:?worktree needs a directory}
  fetch
  git worktree add -q --force --detach "$dir" "$REMOTE_REF"
  copy_outputs "$dir" "$REPO"
  git -C "$dir" rev-parse HEAD > "$STAMP"
  echo "ecosystem-ledger: $BRANCH @ $(git -C "$dir" rev-parse --short HEAD) checked out at $dir"
}

cmd_stage() {
  local dir=${1:?stage needs a directory}
  [ -d "$dir/.git" ] || [ -f "$dir/.git" ] || die "$dir is not a git checkout"
  copy_outputs "$REPO" "$dir"
}

case "${1:-}" in
  pull) shift; cmd_pull "$@" ;;
  status) shift; cmd_status ;;
  worktree) shift; cmd_worktree "$@" ;;
  stage) shift; cmd_stage "$@" ;;
  *) sed -n '2,24p' "$0" | sed 's/^# \{0,1\}//'; exit 2 ;;
esac
