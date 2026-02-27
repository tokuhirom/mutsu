#!/usr/bin/env bash
set -euo pipefail

DRY_RUN=0
AGENT="codex"
FULL_AUTO=0
WIP_FILE="wip.txt"

usage() {
    cat <<USAGE
Usage: $0 [--agent codex|claude] [--full-auto] [--dry-run]

Continuously picks a random failing roast test and processes it.
Loops until pick-next-roast.sh returns no candidates.
Uses wip.txt to coordinate with other instances.

Options:
  --agent <name>  Agent to run in ai-sandbox (codex|claude, default: codex)
  --full-auto     Use codex in --full-auto mode instead of exec (codex only)
  --dry-run       Print commands without executing ai-sandbox
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --agent)
            if [[ $# -lt 2 ]]; then
                echo "Error: --agent requires a value" >&2; exit 1
            fi
            AGENT="$2"; shift 2 ;;
        --full-auto)
            FULL_AUTO=1; shift ;;
        --dry-run)
            DRY_RUN=1; shift ;;
        -h|--help)
            usage; exit 0 ;;
        *)
            echo "Error: unknown argument: $1" >&2; usage; exit 1 ;;
    esac
done

if [[ "$AGENT" != "codex" && "$AGENT" != "claude" ]]; then
    echo "Error: --agent must be codex or claude" >&2; exit 1
fi

# Add entry to wip.txt (create if needed)
wip_add() {
    echo "$1" >> "$WIP_FILE"
}

while true; do
    # Get up to 100 candidates
    CANDIDATES=$(./scripts/pick-next-roast.sh -n 100 2>/dev/null \
        | awk '/^[[:space:]]*[0-9]+[[:space:]]+/ { print $2 }' || true)

    if [[ -z "$CANDIDATES" ]]; then
        echo "No more candidates. Done."
        break
    fi

    # Filter out entries in wip.txt
    if [[ -f "$WIP_FILE" ]]; then
        CANDIDATES=$(comm -23 <(echo "$CANDIDATES" | sort) <(sort "$WIP_FILE") || true)
    fi

    if [[ -z "$CANDIDATES" ]]; then
        echo "All remaining candidates are in wip.txt. Done."
        break
    fi

    # Pick one at random
    FILE=$(echo "$CANDIDATES" | shuf -n 1)

    if [[ -z "$FILE" ]]; then
        echo "No candidate selected. Done."
        break
    fi

    echo "=== Selected: $FILE ==="

    # Register in wip.txt before processing
    wip_add "$FILE"

    RUN_ARGS=(--agent "$AGENT")
    if [[ "$FULL_AUTO" -eq 1 ]]; then
        RUN_ARGS+=(--full-auto)
    fi
    if [[ "$DRY_RUN" -eq 1 ]]; then
        RUN_ARGS+=(--dry-run)
    fi

    ./scripts/ai-run-roast.sh "${RUN_ARGS[@]}" "$FILE" || echo "Failed: $FILE" >&2
done
