#!/usr/bin/env bash
set -euo pipefail

LIMIT=30
DRY_RUN=0
LIST_ONLY=0
RUN_ALL=0
PR_NUMBER=""
AGENT="codex"
FULL_AUTO=0
POLL_INTERVAL_SECONDS=600

usage() {
    cat <<USAGE
Usage: $0 [--agent codex|claude] [--pr <number>] [--all] [--list] [--limit <n>] [--full-auto] [--dry-run]

Options:
  --agent <name>  Agent to run in ai-sandbox (codex|claude, default: codex)
  --pr <number>   Target a specific PR number
  --all           Run for all matching open PRs (conflict/CI failure)
  --list          Show matching PRs and exit
  --limit <n>     Number of open PRs to inspect (default: 30)
  --full-auto     Use codex in --full-auto mode instead of exec (codex only)
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
        --agent)
            if [[ $# -lt 2 ]]; then
                echo "Error: --agent requires a value" >&2
                usage
                exit 1
            fi
            AGENT="$2"
            shift 2
            ;;
        --full-auto)
            FULL_AUTO=1
            shift
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

if [[ "$AGENT" != "codex" && "$AGENT" != "claude" ]]; then
    echo "Error: --agent must be codex or claude" >&2
    usage
    exit 1
fi

require_cmd gh
SCRIPT_DIR_CHECK="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR_CHECK}/.." && pwd)"
STOP_FILE="${REPO_ROOT}/tmp/.stop"
STOP_FILE_PID="${STOP_FILE}.$$"

check_stop_file() {
    if [[ -f "$STOP_FILE_PID" ]]; then
        echo "PID stop file detected ($STOP_FILE_PID). Exiting gracefully."
        rm -f "$STOP_FILE_PID"
        exit 0
    fi
    if [[ -f "$STOP_FILE" ]]; then
        echo "Stop file detected ($STOP_FILE). Exiting gracefully."
        exit 0
    fi
}
if [[ ! -x "${SCRIPT_DIR_CHECK}/ai-sandbox.sh" ]]; then
    echo "Error: ${SCRIPT_DIR_CHECK}/ai-sandbox.sh not found or not executable" >&2
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

run_cmd() {
    if [[ "$AGENT" == "claude" ]]; then
        "$@" 2>&1 | python3 "${SCRIPT_DIR}/stream-json-pretty.py"
    else
        "$@"
    fi
}

build_history_prompt() {
    local branch_name="$1"
    cat <<EOF
Update roast history on branch $branch_name.

Steps:
1. Sync to the latest main:
   git fetch origin
   git checkout main
   git reset --hard origin/main
2. Create and switch to branch: git checkout -b $branch_name
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
    if [[ "$AGENT" == "codex" && "$FULL_AUTO" -eq 1 ]]; then
        cmd=("${SCRIPT_DIR}/ai-sandbox.sh" "$branch_name" codex --full-auto "$prompt")
    elif [[ "$AGENT" == "codex" ]]; then
        cmd=("${SCRIPT_DIR}/ai-sandbox.sh" "$branch_name" codex --dangerously-bypass-approvals-and-sandbox exec "$prompt")
    else
        cmd=("${SCRIPT_DIR}/ai-sandbox.sh" "$branch_name" claude --dangerously-skip-permissions -p --verbose --output-format stream-json "$prompt")
    fi

    echo "No fixable PR found. Running roast history update on: $branch_name"
    echo "Running: ${cmd[*]}"
    if [[ "$DRY_RUN" -eq 1 ]]; then
        return 0
    fi
    run_cmd "${cmd[@]}"
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
                reason: $reason,
                updatedAt,
                priority: (if $reason == "conflict" then 0 else 1 end)
              }
          )
          | sort_by(.priority, .updatedAt)
          | .[]
          | "\(.number)\t\(.reason)\t\(.headRefName)\t\(.title)\t\(.url)"
        '
}

fetch_ci_failure_summary() {
    local pr_number="$1"
    local run_id job_id log_text summary

    # Get the latest failed check run URL from PR status
    run_id="$(gh pr view "$pr_number" \
        --json statusCheckRollup \
        --jq '[.statusCheckRollup[] | select(.conclusion == "FAILURE")] | first | .detailsUrl' \
        2>/dev/null | grep -oP '/runs/\K[0-9]+' | head -1)"

    if [[ -z "$run_id" ]]; then
        return
    fi

    # Get the failed job ID
    job_id="$(gh api "repos/{owner}/{repo}/actions/runs/${run_id}/jobs" --jq '
        [.jobs[] | select(.conclusion == "failure")] | first | .id
    ' 2>/dev/null)"

    if [[ -z "$job_id" ]]; then
        return
    fi

    # Download job log and extract failure lines
    log_text="$(gh api "repos/{owner}/{repo}/actions/jobs/${job_id}/logs" 2>/dev/null)"
    if [[ -z "$log_text" ]]; then
        return
    fi

    summary="$(printf '%s\n' "$log_text" | grep -E '(not ok |FAILED|Failed [1-9]|Wstat: [^0]|Result: FAIL|panicked)' | tail -30)"
    if [[ -n "$summary" ]]; then
        printf '%s\n' "$summary"
    fi
}

build_prompt() {
    local pr_number="$1"
    local reason="$2"
    local head_ref="$3"
    local url="$4"
    local ci_summary="$5"

    cat <<EOF
Fix PR #$pr_number ($head_ref). Reason: $reason.
PR URL: $url
EOF

    if [[ "$reason" == "conflict" ]]; then
        cat <<'EOF'

This PR has merge conflicts with main. Steps:
1. Rebase the branch onto origin/main and resolve conflicts
2. Run make test and make roast to verify no regressions
3. ONLY if both make test and make roast pass (exit 0), force-push the rebased branch
4. If make roast fails, do NOT push. Investigate and fix the failures first.
5. Verify CI passes after pushing
EOF
    elif [[ "$reason" == "ci-fail" ]]; then
        cat <<'EOF'

This PR has CI failures. Steps:
1. First rebase onto origin/main (the failure may already be fixed on main)
2. Build and reproduce the failure locally
3. Fix the code (no test-specific hacks or hardcoded results)
4. Run make test and make roast
5. ONLY if both pass (exit 0), push fixes. Do NOT push if tests fail.
6. Verify CI passes after pushing
EOF
        if [[ -n "$ci_summary" ]]; then
            cat <<EOF

CI failure log (key lines):
$ci_summary
EOF
        fi
    fi

    cat <<'EOF'

After fixing, push and ensure CI passes. If the PR already has auto-merge enabled, it will merge automatically.

Constraints:
- Do not modify anything under roast/
- Keep changes focused on the PR scope
- If PR is already merged/closed, report and stop
EOF
}

run_for_pr() {
    local pr_number="$1"
    local reason="$2"
    local head_ref="$3"
    local url="$4"
    local prompt
    local cmd
    local ci_summary=""

    if [[ "$reason" == "ci-fail" ]]; then
        echo "Fetching CI failure summary for PR #$pr_number..."
        ci_summary="$(fetch_ci_failure_summary "$pr_number")"
    fi

    prompt="$(build_prompt "$pr_number" "$reason" "$head_ref" "$url" "$ci_summary")"
    if [[ "$AGENT" == "codex" && "$FULL_AUTO" -eq 1 ]]; then
        cmd=("${SCRIPT_DIR}/ai-sandbox.sh" "$head_ref" codex --full-auto "$prompt")
    elif [[ "$AGENT" == "codex" ]]; then
        cmd=("${SCRIPT_DIR}/ai-sandbox.sh" "$head_ref" codex --dangerously-bypass-approvals-and-sandbox exec "$prompt")
    else
        cmd=("${SCRIPT_DIR}/ai-sandbox.sh" "$head_ref" claude --dangerously-skip-permissions -p --verbose --output-format stream-json "$prompt")
    fi

    echo "Target PR #$pr_number [$reason] $head_ref"
    echo "URL: $url"
    echo "Running: ${cmd[*]}"
    if [[ "$DRY_RUN" -eq 1 ]]; then
        return 0
    fi
    run_cmd "${cmd[@]}"
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
        check_stop_file
        run_for_pr "$number" "$reason" "$head_ref" "$url"
    done <<<"$CANDIDATES"
    exit 0
fi

while true; do
    check_stop_file
    if [[ -z "$CANDIDATES" ]]; then
        run_history_update
        if [[ "$DRY_RUN" -eq 1 ]]; then
            exit 0
        fi
        echo "No open PRs with conflicts or CI failures were found. Sleeping ${POLL_INTERVAL_SECONDS}s..."
        sleep "$POLL_INTERVAL_SECONDS"
        CANDIDATES="$(collect_candidates_tsv)"
        continue
    fi

    FIRST="$(printf '%s\n' "$CANDIDATES" | head -n 1)"
    IFS=$'\t' read -r number reason head_ref _title url <<<"$FIRST"
    run_for_pr "$number" "$reason" "$head_ref" "$url"
    if [[ "$DRY_RUN" -eq 1 ]]; then
        exit 0
    fi
    CANDIDATES="$(collect_candidates_tsv)"
done
