#!/usr/bin/env bash
set -euo pipefail

LIMIT=30
DRY_RUN=0
LIST_ONLY=0
RUN_ALL=0
PR_NUMBER=""
POLL_INTERVAL_SECONDS=600

usage() {
    cat <<USAGE
Usage: $0 [--pr <number>] [--all] [--list] [--limit <n>] [--dry-run]

Options:
  --pr <number>   Target a specific PR number
  --all           Run for all matching open PRs (conflict/CI failure)
  --list          Show matching PRs and exit
  --limit <n>     Number of open PRs to inspect (default: 30)
  --dry-run       Print ai-sandbox command without executing it
  -h, --help      Show this help
USAGE
}

is_positive_integer() {
    [[ "$1" =~ ^[1-9][0-9]*$ ]]
}

require_cmd() {
    command -v "$1" >/dev/null 2>&1 || {
        echo "Error: required command not found: $1" >&2
        exit 1
    }
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --pr)
            if [[ $# -lt 2 ]]; then
                echo "Error: --pr requires a value" >&2
                usage
                exit 1
            fi
            if ! is_positive_integer "$2"; then
                echo "Error: --pr requires a positive integer" >&2
                usage
                exit 1
            fi
            PR_NUMBER="$2"
            shift 2
            ;;
        --all)
            RUN_ALL=1
            shift
            ;;
        --list)
            LIST_ONLY=1
            shift
            ;;
        --limit)
            if [[ $# -lt 2 ]]; then
                echo "Error: --limit requires a value" >&2
                usage
                exit 1
            fi
            if ! is_positive_integer "$2"; then
                echo "Error: --limit requires a positive integer" >&2
                usage
                exit 1
            fi
            LIMIT="$2"
            shift 2
            ;;
        --dry-run)
            DRY_RUN=1
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Error: unknown argument: $1" >&2
            usage
            exit 1
            ;;
    esac
done

if [[ -n "$PR_NUMBER" && "$RUN_ALL" -eq 1 ]]; then
    echo "Error: --pr and --all cannot be used together" >&2
    usage
    exit 1
fi

require_cmd gh
require_cmd ai-sandbox

build_history_prompt() {
    local branch_name="$1"
    cat <<EOF
Update roast history on branch $branch_name.

Follow the repository PR workflow in CLAUDE.md and handle this end-to-end:
1. Checkout main and sync latest origin/main
2. Create and switch to branch: $branch_name
3. Run ./scripts/roast-history.sh --commit
4. If there are no changes, report and stop (do not open a PR)
5. Otherwise push the branch and open a PR with gh pr create
6. Enable auto-merge for the PR

Constraints:
- Keep the changes limited to roast history artifacts
- Do not modify anything under roast/
EOF
}

run_history_update() {
    local timestamp
    local branch_name
    local prompt
    local cmd

    timestamp="$(date +%Y%m%d%H%M)"
    branch_name="update-history-${timestamp}"
    prompt="$(build_history_prompt "$branch_name")"
    cmd=(ai-sandbox "$branch_name" codex exec "$prompt")

    echo "No fixable PR found. Running roast history update on: $branch_name"
    echo "Running: ${cmd[*]}"
    if [[ "$DRY_RUN" -eq 1 ]]; then
        return 0
    fi
    "${cmd[@]}"
}

collect_candidates_tsv() {
    gh pr list \
        --state open \
        --limit "$LIMIT" \
        --json number,title,headRefName,url,mergeable,mergeStateStatus,statusCheckRollup,updatedAt \
        --jq '
          map(
            . as $pr
            | ([.statusCheckRollup[]? | select(.__typename == "CheckRun" and .conclusion == "FAILURE")] | length) as $check_failures
            | ([.statusCheckRollup[]? | select(.__typename == "StatusContext" and .conclusion == "FAILURE")] | length) as $status_failures
            | ($check_failures + $status_failures) as $failures
            | (if .mergeable == "CONFLICTING" or .mergeStateStatus == "DIRTY" then "conflict"
               elif $failures > 0 then "ci-fail"
               else null end) as $reason
            | select($reason != null)
            | {
                number,
                title,
                headRefName,
                url,
                reason,
                updatedAt,
                priority: (if $reason == "conflict" then 0 else 1 end)
              }
          )
          | sort_by(.priority, .updatedAt)
          | .[]
          | "\(.number)\t\(.reason)\t\(.headRefName)\t\(.title)\t\(.url)"
        '
}

build_prompt() {
    local pr_number="$1"
    local reason="$2"
    local head_ref="$3"
    local url="$4"
    cat <<EOF
Fix PR #$pr_number ($head_ref). Reason: $reason.
PR URL: $url

Follow the PR workflow in CLAUDE.md and handle this end-to-end:
1. Inspect PR checks and merge/conflict status
2. If needed, rebase/merge main and resolve conflicts
3. Reproduce failing checks locally
4. Implement minimal, general fixes (no test-specific hacks)
5. Run make test and make roast
6. Commit and push fixes to the PR branch
7. Ensure CI passes and merge the PR

Constraints:
- Do not modify anything under roast/
- Keep changes focused on the PR scope
- If PR is already merged/closed, report and stop for that PR
EOF
}

run_for_pr() {
    local pr_number="$1"
    local reason="$2"
    local head_ref="$3"
    local url="$4"
    local prompt
    local cmd

    prompt="$(build_prompt "$pr_number" "$reason" "$head_ref" "$url")"
    cmd=(ai-sandbox "pr-${pr_number}" codex exec "$prompt")

    echo "Target PR #$pr_number [$reason] $head_ref"
    echo "URL: $url"
    echo "Running: ${cmd[*]}"
    if [[ "$DRY_RUN" -eq 1 ]]; then
        return 0
    fi
    "${cmd[@]}"
}

if [[ -n "$PR_NUMBER" ]]; then
    PR_JSON="$(
        gh pr view "$PR_NUMBER" \
            --json number,title,headRefName,url,mergeable,mergeStateStatus,statusCheckRollup \
            --jq '
              . as $pr
              | ([.statusCheckRollup[]? | select(.__typename == "CheckRun" and .conclusion == "FAILURE")] | length) as $check_failures
              | ([.statusCheckRollup[]? | select(.__typename == "StatusContext" and .conclusion == "FAILURE")] | length) as $status_failures
              | ($check_failures + $status_failures) as $failures
              | (if .mergeable == "CONFLICTING" or .mergeStateStatus == "DIRTY" then "conflict"
                 elif $failures > 0 then "ci-fail"
                 else "manual" end) as $reason
              | "\(.number)\t\($reason)\t\(.headRefName)\t\(.title)\t\(.url)"
            '
    )"

    if [[ -z "$PR_JSON" ]]; then
        echo "Error: failed to inspect PR #$PR_NUMBER" >&2
        exit 1
    fi

    IFS=$'\t' read -r number reason head_ref _title url <<<"$PR_JSON"
    run_for_pr "$number" "$reason" "$head_ref" "$url"
    exit 0
fi

CANDIDATES="$(collect_candidates_tsv)"

if [[ "$LIST_ONLY" -eq 1 ]]; then
    if [[ -z "$CANDIDATES" ]]; then
        echo "No open PRs with conflicts or CI failures were found."
        exit 0
    fi
    echo "Matched PRs (priority order):"
    while IFS=$'\t' read -r number reason head_ref title url; do
        echo "#$number [$reason] $head_ref :: $title"
        echo "  $url"
    done <<<"$CANDIDATES"
    exit 0
fi

if [[ "$RUN_ALL" -eq 1 ]]; then
    if [[ -z "$CANDIDATES" ]]; then
        echo "No open PRs with conflicts or CI failures were found."
        exit 0
    fi
    while IFS=$'\t' read -r number reason head_ref _title url; do
        run_for_pr "$number" "$reason" "$head_ref" "$url"
    done <<<"$CANDIDATES"
    exit 0
fi

while true; do
    if [[ -z "$CANDIDATES" ]]; then
        run_history_update
        echo "No open PRs with conflicts or CI failures were found. Sleeping ${POLL_INTERVAL_SECONDS}s..."
        sleep "$POLL_INTERVAL_SECONDS"
        CANDIDATES="$(collect_candidates_tsv)"
        continue
    fi

    FIRST="$(printf '%s\n' "$CANDIDATES" | head -n 1)"
    IFS=$'\t' read -r number reason head_ref _title url <<<"$FIRST"
    run_for_pr "$number" "$reason" "$head_ref" "$url"
    CANDIDATES="$(collect_candidates_tsv)"
done
