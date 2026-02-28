#!/usr/bin/env bash
set -euo pipefail

DRY_RUN=0
AGENT="codex"
WIP_FILE="wip.txt"

usage() {
    cat <<USAGE
Usage: $0 [--agent codex|claude] [--dry-run]

Continuously picks a random failing roast test and processes it.
Loops until pick-next-roast.sh returns no candidates.
Uses wip.txt to coordinate with other instances.

Options:
  --agent <name>  Agent to run in ai-sandbox (codex|claude, default: codex)
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

# Remove entry from wip.txt
wip_remove() {
    if [[ -f "$WIP_FILE" ]]; then
        local tmp
        tmp=$(grep -vxF "$1" "$WIP_FILE" || true)
        echo "$tmp" > "$WIP_FILE"
    fi
}

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
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

# raku フィルターを生成（raku で通らないテストを除外するため）
if [[ ! -f "tmp/roast-raku-pass.txt" ]]; then
    echo "Generating raku filter (tmp/roast-raku-pass.txt)..."
    ./scripts/roast-raku-check.sh || true
fi

while true; do
    check_stop_file
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

    if [[ "$DRY_RUN" -eq 1 ]]; then
        ./scripts/ai-run-roast.sh --agent "$AGENT" --dry-run "$FILE"
        wip_remove "$FILE"
        exit 0
    else
        ./scripts/ai-run-roast.sh --agent "$AGENT" "$FILE" || echo "Failed: $FILE" >&2
    fi

    # Remove from wip.txt after processing
    wip_remove "$FILE"
done
