#!/usr/bin/env bash
set -euo pipefail

DRY_RUN=0
AGENT="codex"

usage() {
    cat <<USAGE
Usage: $0 [--dry-run] [--agent codex|claude] <roast-file>

Options:
  --dry-run       Print the ai-sandbox command without executing it
  --agent <name>  Agent to run in ai-sandbox (codex|claude, default: codex)
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run)
            DRY_RUN=1
            shift
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
        -h|--help)
            usage
            exit 0
            ;;
        --)
            shift
            break
            ;;
        -*)
            echo "Error: unknown argument: $1" >&2
            usage
            exit 1
            ;;
        *)
            break
            ;;
    esac
done

if [[ $# -ne 1 ]]; then
    echo "Error: roast file path is required" >&2
    usage
    exit 1
fi

FILE="$1"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
STOP_FILE="${REPO_ROOT}/tmp/.stop"

check_stop_file() {
    if [[ -f "$STOP_FILE" ]]; then
        echo "Stop file detected ($STOP_FILE). Exiting gracefully."
        rm -f "$STOP_FILE"
        exit 0
    fi
}

if [[ "$AGENT" != "codex" && "$AGENT" != "claude" ]]; then
    echo "Error: --agent must be codex or claude" >&2
    usage
    exit 1
fi

if [[ ! -f "$FILE" ]]; then
    echo "Error: file not found: $FILE" >&2
    exit 1
fi

PROMPT=$(cat <<EOF_PROMPT
$FILE should pass. Follow the roast workflow in CLAUDE.md.

Required investigation steps:
1. Run with raku to confirm expected behavior: raku $FILE
2. Check raku AST for a minimal relevant snippet: raku --target=ast -e '...'
3. Check mutsu AST: timeout 30 target/release/mutsu --dump-ast $FILE
4. Run with mutsu: timeout 30 target/release/mutsu $FILE
5. Identify the behavioral gap and implement the missing feature as a general solution

Constraints:
- Do not modify anything under roast/
- No test-specific hacks or hardcoded outputs

After implementing:
- Verify with cargo build --release && timeout 30 target/release/mutsu $FILE
- Add regression tests under t/ if needed
- Run cargo clippy -- -D warnings and fix any warnings
- Run cargo fmt to format the code
- Run make test and make roast to check regressions (these use release build)
- ONLY proceed to next steps if both make test and make roast pass (exit 0)
- If it passes, append to roast-whitelist.txt while keeping sort order
- before opening a PR, merge the latest remote main with: git fetch origin && git merge origin/main
- resolve merge conflicts if any, rerun relevant checks, then commit and push
- open a PR with gh pr create
- enable auto merge
EOF_PROMPT
)
MAX_RETRIES=3
RETRY_DELAY=30

if [[ "$AGENT" == "codex" ]]; then
    CMD=("${SCRIPT_DIR}/ai-sandbox.sh" --recreate "$FILE" codex --dangerously-bypass-approvals-and-sandbox exec "$PROMPT")
else
    CMD=("${SCRIPT_DIR}/ai-sandbox.sh" --recreate "$FILE" claude --dangerously-skip-permissions -p --verbose --output-format stream-json "$PROMPT")
fi

echo "Selected file: $FILE"
echo "Running: ${CMD[*]}"

if [[ "$DRY_RUN" -eq 1 ]]; then
    exit 0
fi

run_cmd() {
    if [[ "$AGENT" == "claude" ]]; then
        "${CMD[@]}" 2>&1 | python3 "${SCRIPT_DIR}/stream-json-pretty.py"
    else
        "${CMD[@]}"
    fi
}

for attempt in $(seq 1 "$MAX_RETRIES"); do
    check_stop_file
    if run_cmd; then
        exit 0
    fi
    if [[ "$attempt" -lt "$MAX_RETRIES" ]]; then
        echo "Attempt $attempt/$MAX_RETRIES failed. Retrying in ${RETRY_DELAY}s..." >&2
        sleep "$RETRY_DELAY"
    fi
done

echo "All $MAX_RETRIES attempts failed for $FILE" >&2
exit 1
