#!/usr/bin/env bash
set -euo pipefail

COUNT=300
START_LINE=20
END_LINE=300
DRY_RUN=0

usage() {
    cat <<USAGE
Usage: $0 [-n count] [-s start_line] [-e end_line] [--dry-run]

Options:
  -n <count>      Passed to ./scripts/pick-next-roast.sh (default: 300)
  -s <start_line> Start line for random selection (default: 20)
  -e <end_line>   End line for random selection (default: 300)
  --start <n>     Same as -s
  --end <n>       Same as -e
  --dry-run       Print the ai-sandbox command without executing it
USAGE
}

is_positive_integer() {
    [[ "$1" =~ ^[1-9][0-9]*$ ]]
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        -n)
            if [[ $# -lt 2 ]]; then
                echo "Error: -n requires a value" >&2
                usage
                exit 1
            fi
            if ! is_positive_integer "$2"; then
                echo "Error: -n requires a positive integer" >&2
                usage
                exit 1
            fi
            COUNT="$2"
            shift 2
            ;;
        -s|--start)
            if [[ $# -lt 2 ]]; then
                echo "Error: $1 requires a value" >&2
                usage
                exit 1
            fi
            if ! is_positive_integer "$2"; then
                echo "Error: $1 requires a positive integer" >&2
                usage
                exit 1
            fi
            START_LINE="$2"
            shift 2
            ;;
        -e|--end)
            if [[ $# -lt 2 ]]; then
                echo "Error: $1 requires a value" >&2
                usage
                exit 1
            fi
            if ! is_positive_integer "$2"; then
                echo "Error: $1 requires a positive integer" >&2
                usage
                exit 1
            fi
            END_LINE="$2"
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

if (( START_LINE > END_LINE )); then
    echo "Error: start line must be less than or equal to end line" >&2
    usage
    exit 1
fi

SELECTED_LINE=$(
    ./scripts/pick-next-roast.sh -n "$COUNT" \
    | sed -n "${START_LINE},${END_LINE}p" \
    | shuf -n 1
)
FILE=$(echo "$SELECTED_LINE" | sed -E 's/^[[:space:]]*[0-9]+[[:space:]]+//')

if [[ -z "$SELECTED_LINE" ]]; then
    echo "Failed to select a line from range ${START_LINE}-${END_LINE}" >&2
    exit 1
fi

if [[ -z "$FILE" || "$FILE" == "$SELECTED_LINE" ]]; then
    echo "Failed to parse file path from: $SELECTED_LINE" >&2
    exit 1
fi

PROMPT=$(cat <<EOF
$FILE should pass. Follow the roast workflow in CLAUDE.md.

Required investigation steps:
1. Run with raku to confirm expected behavior: raku $FILE
2. Check raku AST for a minimal relevant snippet: raku --target=ast -e '...'
3. Check mutsu AST: timeout 30 target/debug/mutsu --dump-ast $FILE
4. Run with mutsu: timeout 30 target/debug/mutsu $FILE
5. Identify the behavioral gap and implement the missing feature as a general solution

Constraints:
- Do not modify anything under roast/
- No test-specific hacks or hardcoded outputs

After implementing:
- Verify with cargo build && timeout 30 target/debug/mutsu $FILE
- Add regression tests under t/ if needed
- Run cargo clippy -- -D warnings and fix any warnings
- Run cargo fmt to format the code
- Run make test and make roast to check regressions
- If it passes, append to roast-whitelist.txt while keeping sort order
- commit, push, and open a PR with gh pr create
- enable auto merge
EOF
)
CMD=(ai-sandbox "$FILE" codex exec "$PROMPT")

echo "Selected file: $FILE"
echo "Running: ${CMD[*]}"

if [[ "$DRY_RUN" -eq 1 ]]; then
    exit 0
fi

"${CMD[@]}"
