#!/usr/bin/env bash
set -euo pipefail

DRY_RUN=0

usage() {
    cat <<USAGE
Usage: $0 [--dry-run] <roast-file>

Options:
  --dry-run       Print the ai-sandbox command without executing it
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run)
            DRY_RUN=1
            shift
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

if [[ ! -f "$FILE" ]]; then
    echo "Error: file not found: $FILE" >&2
    exit 1
fi

PROMPT=$(cat <<EOF_PROMPT
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
EOF_PROMPT
)
CMD=(ai-sandbox "$FILE" codex exec "$PROMPT")

echo "Selected file: $FILE"
echo "Running: ${CMD[*]}"

if [[ "$DRY_RUN" -eq 1 ]]; then
    exit 0
fi

"${CMD[@]}"
