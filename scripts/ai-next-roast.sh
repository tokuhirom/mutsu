#!/usr/bin/env bash
set -euo pipefail

COUNT=300
DRY_RUN=0

usage() {
    cat <<USAGE
Usage: $0 [-n count] [--dry-run]

Options:
  -n <count>   Passed to ./scripts/pick-next-roast.sh (default: 300)
  --dry-run    Print the ai-sandbox command without executing it
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        -n)
            if [[ $# -lt 2 ]]; then
                echo "Error: -n requires a value" >&2
                usage
                exit 1
            fi
            COUNT="$2"
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

LAST_LINE=$(./scripts/pick-next-roast.sh -n "$COUNT" | tail -n 1)
FILE=$(echo "$LAST_LINE" | sed -E 's/^[[:space:]]*[0-9]+[[:space:]]+//')

if [[ -z "$FILE" || "$FILE" == "$LAST_LINE" ]]; then
    echo "Failed to parse file path from: $LAST_LINE" >&2
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
- Run make test and make roast to check regressions
- If it passes, append to roast-whitelist.txt while keeping sort order
- Create a branch, commit, push, and open a PR with gh pr create
EOF
)
CMD=(ai-sandbox "$FILE" codex exec "$PROMPT")

echo "Selected file: $FILE"
echo "Running: ${CMD[*]}"

if [[ "$DRY_RUN" -eq 1 ]]; then
    exit 0
fi

"${CMD[@]}"
