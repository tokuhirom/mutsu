#!/usr/bin/env bash
#
# update-vendor.sh — manage the vendored upstream Raku sources.
#
# The trees listed in vendor.lock (roast, raku-doc, old-design-docs) are copied
# verbatim from upstream Raku repositories and pinned to a specific commit. They
# are vendored (not git submodules) on purpose, so that this workspace has no
# upstream remote and cannot accidentally push a PR / open an issue against the
# Raku org repos. See docs/vendoring.md.
#
# Usage:
#   scripts/update-vendor.sh --check              Show how far behind upstream
#                                                 each vendored tree is. No writes.
#   scripts/update-vendor.sh <name> [ref]         Re-vendor <name> from upstream
#                                                 at <ref> (default: branch HEAD)
#                                                 and update vendor.lock.
#   scripts/update-vendor.sh --all                Re-vendor every tree at its
#                                                 branch HEAD.
#
#   <name> is one of the names in the first column of vendor.lock.
#   <ref>  is any git ref (branch, tag, or full/short commit) valid upstream.
#
# Examples:
#   scripts/update-vendor.sh --check
#   scripts/update-vendor.sh raku-doc
#   scripts/update-vendor.sh roast b2cbe8a42eaf9a044dc95f544428bffdeb2870c7
#
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LOCK="$REPO_ROOT/vendor.lock"

# Temp dirs are cleaned up on exit (robust against early exits and set -u).
_CLEANUP=""
cleanup() { [[ -n "$_CLEANUP" ]] && rm -rf "$_CLEANUP"; }
trap cleanup EXIT

if [[ ! -f "$LOCK" ]]; then
  echo "error: $LOCK not found" >&2
  exit 1
fi

# Read the data rows (skip comments / blank lines) into an array.
# Each row: name<TAB>dir<TAB>url<TAB>branch<TAB>commit<TAB>date
read_rows() {
  grep -vE '^[[:space:]]*(#|$)' "$LOCK"
}

lookup() { # $1 = name -> prints the row, or nothing
  read_rows | awk -F'\t' -v n="$1" '$1==n {print; exit}'
}

remote_head() { # $1 = url  $2 = branch  -> full sha of upstream branch HEAD
  git ls-remote "$1" "refs/heads/$2" | awk '{print $1}'
}

do_check() {
  local any_behind=0
  printf '%-16s %-10s %-10s %s\n' "NAME" "PINNED" "UPSTREAM" "STATUS"
  while IFS=$'\t' read -r name dir url branch commit date; do
    [[ -z "${name:-}" ]] && continue
    local up
    up="$(remote_head "$url" "$branch" || true)"
    if [[ -z "$up" ]]; then
      printf '%-16s %-10s %-10s %s\n' "$name" "${commit:0:8}" "?" "could not reach $url"
      continue
    fi
    if [[ "$up" == "$commit" ]]; then
      printf '%-16s %-10s %-10s %s\n' "$name" "${commit:0:8}" "${up:0:8}" "up to date"
    else
      printf '%-16s %-10s %-10s %s\n' "$name" "${commit:0:8}" "${up:0:8}" "BEHIND ($branch)"
      any_behind=1
    fi
  done < <(read_rows)
  return $any_behind
}

update_one() { # $1 = name  $2 = ref (optional)
  local name="$1" ref="${2:-}"
  local row; row="$(lookup "$name")"
  if [[ -z "$row" ]]; then
    echo "error: '$name' is not listed in vendor.lock" >&2
    echo "known names: $(read_rows | cut -f1 | tr '\n' ' ')" >&2
    exit 1
  fi
  local dir url branch
  dir="$(printf '%s' "$row" | cut -f2)"
  url="$(printf '%s' "$row" | cut -f3)"
  branch="$(printf '%s' "$row" | cut -f4)"
  [[ -z "$ref" ]] && ref="$branch"

  local target="$REPO_ROOT/$dir"
  if [[ ! -d "$target" ]]; then
    echo "error: vendored dir '$target' does not exist" >&2
    exit 1
  fi

  local tmp; tmp="$(mktemp -d)"
  _CLEANUP="$tmp"

  echo ">> cloning $url ($branch) ..."
  git clone --quiet --filter=blob:none "$url" "$tmp/src"
  echo ">> checking out $ref ..."
  git -C "$tmp/src" checkout --quiet "$ref"
  local sha date
  sha="$(git -C "$tmp/src" rev-parse HEAD)"
  date="$(git -C "$tmp/src" show -s --format=%cd --date=short HEAD)"
  echo ">> $name -> $sha ($date)"

  echo ">> syncing into $dir/ ..."
  # --delete makes the vendored tree an exact copy of upstream at this commit.
  # Exclude .git; upstream-gitignored local artifacts (e.g. roast/.precomp) are
  # not present in the source tree, so they are removed too — they regenerate.
  rsync -a --delete --exclude='.git' "$tmp/src/" "$target/"

  # Rewrite this tree's row in the lock with the new commit + date.
  local newrow
  newrow="$(printf '%s\t%s\t%s\t%s\t%s\t%s' "$name" "$dir" "$url" "$branch" "$sha" "$date")"
  local tmplock; tmplock="$(mktemp)"
  awk -F'\t' -v n="$name" -v repl="$newrow" '
    /^[[:space:]]*(#|$)/ { print; next }
    $1==n { print repl; next }
    { print }
  ' "$LOCK" > "$tmplock"
  mv "$tmplock" "$LOCK"
  echo ">> vendor.lock updated for $name"
  echo ">> review: git status $dir/ vendor.lock"
  if [[ "$name" == "roast" ]]; then
    echo ">> NOTE: roast changed — re-verify roast-whitelist.txt entries that upstream touched."
  fi

  rm -rf "$tmp"; _CLEANUP=""
}

main() {
  case "${1:-}" in
    ""|-h|--help)
      sed -n '2,40p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
    --check)
      do_check
      ;;
    --all)
      while IFS=$'\t' read -r name _rest; do
        [[ -z "${name:-}" ]] && continue
        update_one "$name"
      done < <(read_rows)
      ;;
    *)
      update_one "$1" "${2:-}"
      ;;
  esac
}

main "$@"
