#!/usr/bin/env bash
#
# Derive the `working` label on tokuhirom/mutsu issues from the claim log.
#
# docs/issue-workflow.md makes the comment thread the record of who is working
# on an issue and the `working` label only a fast filter over it. Agents kept
# posting the `Claiming:` comment and then never adding the label (#8033 was
# claimed for six hours with no label; #8094 for an hour), which left the fast
# filter lying in the one direction that costs a build slot: an issue that
# looks free while somebody is on it.
#
# So stop asking an agent to keep two things in sync and derive one from the
# other. This replays an issue's whole comment log, computes the set of live
# claims, and makes the label agree:
#
#   * any live claim  -> the issue carries `working`
#   * none            -> it does not
#
# It is idempotent, so an agent that DOES add the label by hand changes nothing,
# and a run that is triggered twice does the same work twice with one outcome.
#
#   .github/scripts/sync-working-label.sh --self-test   # parser cases, no API
#   .github/scripts/sync-working-label.sh <issue>       # reconcile one issue
#   .github/scripts/sync-working-label.sh --all         # every open issue
#
# With RELEASE_STALE=1 (the scheduled run), `--all` also breaks a claim its
# session died holding, by the evidence-based test docs/issue-workflow.md
# already defines for the ecosystem lock board: older than $STALE_HOURS AND no
# such branch on origin. It posts a real `Releasing:` comment rather than just
# dropping the label, because the log -- not the label -- is the record.
#
# Needs: gh (authenticated), jq is not required (gh --jq covers it), awk, date.
set -uo pipefail

REPO="${REPO:-tokuhirom/mutsu}"
LABEL="working"
STALE_HOURS="${STALE_HOURS:-24}"
RELEASE_STALE="${RELEASE_STALE:-0}"

# ---------------------------------------------------------------- the parser

# Replay a claim log and print the claims that are still live.
#
# stdin : one TSV line per comment, oldest first -- <created_at>\t<first line>
# stdout: one live claim per line -- <branch>\t<created_at of the claim>
#
# Only the FIRST line of a comment is inspected, and only its first two
# whitespace-separated fields, which is what makes the format survive the prose
# agents append after it ("Releasing: br - fixed in #8114", a Claude Code
# attribution footer, a paragraph of findings).
#
# `Locking:` / `Unlocking:` (the ecosystem lock board, #7884) are deliberately
# NOT claims: that board locks distributions, not the issue it lives on.
live_claims() {
  awk -F'\t' '
    {
      ts = $1; line = $2
      sub(/\r$/, "", line)
      sub(/^[ \t]+/, "", line)
      n = split(line, f, /[ \t]+/)
      if (n < 2 || f[2] == "") next
      if (f[1] == "Claiming:") {
        if (!(f[2] in seen)) { order[++count] = f[2]; seen[f[2]] = 1 }
        claimed_at[f[2]] = ts
        live[f[2]] = 1
      } else if (f[1] == "Releasing:") {
        live[f[2]] = 0
      }
    }
    END {
      for (i = 1; i <= count; i++) {
        b = order[i]
        if (live[b]) printf "%s\t%s\n", b, claimed_at[b]
      }
    }
  '
}

# ------------------------------------------------------------------- the API

comment_log() { # comment_log <issue>
  gh api --paginate "repos/$REPO/issues/$1/comments" \
    --jq '.[] | [.created_at, ((.body // "") | gsub("\r"; "") | split("\n")[0])] | @tsv'
}

issue_labels() { # issue_labels <issue>
  gh api "repos/$REPO/issues/$1" --jq '.labels[].name'
}

has_label() { # has_label <label> <<< "<labels>"
  grep -qxF "$1"
}

branch_exists() { # branch_exists <branch>
  gh api "repos/$REPO/git/ref/heads/$1" >/dev/null 2>&1
}

hours_since() { # hours_since <iso8601>
  local then now
  then=$(date -u -d "$1" +%s 2>/dev/null) || { echo 0; return; }
  now=$(date -u +%s)
  echo $(( (now - then) / 3600 ))
}

# ------------------------------------------------------------------ the sync

# Break a claim whose session died. Both halves of the test are checkable by
# anyone, which is what keeps it from becoming a judgement call: old enough,
# and nothing was ever pushed under the name the claim gave.
release_if_stale() { # release_if_stale <issue> <branch> <claimed_at>
  local n="$1" branch="$2" ts="$3" age
  [ "$RELEASE_STALE" = 1 ] || return 1
  age=$(hours_since "$ts")
  [ "$age" -ge "$STALE_HOURS" ] || return 1
  branch_exists "$branch" && return 1

  echo "  #$n: auto-releasing stale claim '$branch' (${age}h old, no such branch on origin)"
  gh issue comment "$n" --repo "$REPO" --body "$(printf '%s\n' \
    "Releasing: $branch" \
    "" \
    "Auto-released by \`.github/workflows/claim-label.yml\`: this claim is ${age}h old and no branch \`$branch\` exists on \`origin\`, which is the stale-claim test in [docs/issue-workflow.md](https://github.com/$REPO/blob/main/docs/issue-workflow.md). The \`$LABEL\` label is being removed so the issue returns to the queue." \
    "" \
    "If you are still on it, claim it again.")" >/dev/null
  return 0
}

sync_issue() { # sync_issue <issue>
  local n="$1" labels claims live=0 line branch ts

  labels=$(issue_labels "$n" 2>/dev/null) || { echo "  #$n: unreadable, skipped"; return 0; }
  # The lock board is infrastructure: its comments lock distributions, not it.
  if printf '%s\n' "$labels" | has_label "ecosystem:lock"; then return 0; fi

  claims=$(comment_log "$n" | live_claims)

  while IFS=$'\t' read -r branch ts; do
    [ -n "$branch" ] || continue
    if release_if_stale "$n" "$branch" "$ts"; then continue; fi
    live=$((live + 1))
    echo "  #$n: live claim '$branch' ($(hours_since "$ts")h)"
  done <<EOF
$claims
EOF

  if printf '%s\n' "$labels" | has_label "$LABEL"; then
    if [ "$live" -eq 0 ]; then
      echo "  #$n: no live claim -> removing $LABEL"
      gh issue edit "$n" --repo "$REPO" --remove-label "$LABEL"
    fi
  elif [ "$live" -gt 0 ]; then
    echo "  #$n: $live live claim(s) -> adding $LABEL"
    gh issue edit "$n" --repo "$REPO" --add-label "$LABEL"
  fi
}

sync_all() {
  local n
  # Open issues only: a closed one is out of the queue whatever its label says.
  for n in $(gh issue list --repo "$REPO" --state open --limit 300 --json number --jq '.[].number'); do
    sync_issue "$n"
  done
}

# ------------------------------------------------------------- the self-test

self_test() {
  local failures=0
  check() { # check <label> <expected live branches, space separated> <log lines...>
    local label="$1" expected="$2"; shift 2
    local got
    got=$(printf '%s\n' "$@" | live_claims | cut -f1 | tr '\n' ' ' | sed -E 's/ +$//')
    if [ "$got" != "$expected" ]; then
      echo "not ok - $label (expected '$expected', got '$got')" >&2
      failures=$((failures + 1))
    else
      echo "ok - $label"
    fi
  }

  ts='2026-09-12T01:00:00Z'
  check 'a bare claim is live' 'br-a' \
    "$ts	Claiming: br-a"
  check 'claim then release' '' \
    "$ts	Claiming: br-a" \
    "$ts	Releasing: br-a"
  check 'release carrying prose still releases' '' \
    "$ts	Claiming: br-a" \
    "$ts	Releasing: br-a - fixed in #8114."
  check 'a losing claim releases only itself' 'br-a' \
    "$ts	Claiming: br-a" \
    "$ts	Claiming: br-b" \
    "$ts	Releasing: br-b"
  check 'both claims live until released' 'br-a br-b' \
    "$ts	Claiming: br-a" \
    "$ts	Claiming: br-b"
  check 'a re-claim after release is live again' 'br-a' \
    "$ts	Claiming: br-a" \
    "$ts	Releasing: br-a" \
    "$ts	Claiming: br-a"
  check 'ordinary comments are not claims' '' \
    "$ts	Investigation: reproduced both boundaries from the issue." \
    "$ts	The fix is in the compiler, not the VM."
  check 'a claim buried below the first line does not count' '' \
    "$ts	Some preamble - Claiming: br-a"
  check 'lock-board lines are not issue claims' '' \
    "$ts	Locking: String--Utils br-a" \
    "$ts	Unlocking: String--Utils br-a"
  check 'leading whitespace is tolerated' 'br-a' \
    "$ts	  Claiming: br-a"
  check 'a keyword with no branch is ignored' '' \
    "$ts	Claiming:" \
    "$ts	Releasing:"
  check 'an empty log has no claims' '' ''

  # The two shapes that actually caused this script to exist.
  check '#8033: three slices, the last one still open' 'feat/issue-8033-execution-tree-lowering' \
    "$ts	Claiming: docs/issue-8033-regex-ast-design" \
    "$ts	Releasing: docs/issue-8033-regex-ast-design" \
    "$ts	Claiming: feat/issue-8033-shared-regex-tree" \
    "$ts	Releasing: feat/issue-8033-shared-regex-tree" \
    "$ts	Claiming: feat/issue-8033-execution-tree-lowering"
  check '#8094: claim and release both carry a footer' '' \
    "$ts	Claiming: claude/laughing-clarke-6i0912" \
    "$ts	Releasing: claude/laughing-clarke-6i0912 - fixed in https://github.com/tokuhirom/mutsu/pull/8114."

  if [ "$failures" -ne 0 ]; then
    echo "sync-working-label self-test: $failures failure(s)" >&2
    return 1
  fi
  echo "sync-working-label self-test: all cases pass"
}

# ------------------------------------------------------------------ dispatch

case "${1:-}" in
  --self-test) self_test; exit $? ;;
  --all)       sync_all; exit 0 ;;
  '')          echo "usage: $0 --self-test | --all | <issue-number>" >&2; exit 2 ;;
  *)           sync_issue "$1"; exit 0 ;;
esac
