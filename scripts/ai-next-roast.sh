#!/usr/bin/env bash
set -euo pipefail

COUNT=300
START_LINE=20
END_LINE=300
DRY_RUN=0
SEQUENTIAL=0

usage() {
    cat <<USAGE
Usage: $0 [-n count] [-s start_line] [-e end_line] [--sequential] [--dry-run]

Options:
  -n <count>      Passed to ./scripts/pick-next-roast.sh (default: 300)
  -s <start_line> Start line for candidate selection (default: 20)
  -e <end_line>   End line for candidate selection (default: 300)
  --start <n>     Same as -s
  --end <n>       Same as -e
  --sequential    Process all selected candidates from -s to -e in order
  --dry-run       Print commands without executing ai-sandbox
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
        --sequential)
            SEQUENTIAL=1
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

if (( START_LINE > END_LINE )); then
    echo "Error: start line must be less than or equal to end line" >&2
    usage
    exit 1
fi

CANDIDATES=$(./scripts/pick-next-roast.sh -n "$COUNT" | awk '/^[[:space:]]*[0-9]+[[:space:]]+/' || true)
SELECTED_LINES=$(echo "$CANDIDATES" | sed -n "${START_LINE},${END_LINE}p")

if [[ -z "$SELECTED_LINES" ]]; then
    echo "Failed to select candidate lines from range ${START_LINE}-${END_LINE}" >&2
    exit 1
fi

if [[ "$SEQUENTIAL" -eq 1 ]]; then
    failed=0
    while IFS= read -r line; do
        [[ -z "$line" ]] && continue
        file=$(echo "$line" | sed -E 's/^[[:space:]]*[0-9]+[[:space:]]+//')
        if [[ -z "$file" || "$file" == "$line" ]]; then
            echo "Skipping unparsable line: $line" >&2
            continue
        fi

        if [[ "$DRY_RUN" -eq 1 ]]; then
            ./scripts/ai-run-roast.sh --dry-run "$file"
        else
            if ! ./scripts/ai-run-roast.sh "$file"; then
                echo "Failed: $file" >&2
                failed=$((failed + 1))
            fi
        fi
    done <<< "$SELECTED_LINES"

    if (( failed > 0 )); then
        echo "Completed with $failed failure(s)." >&2
        exit 1
    fi
    exit 0
fi

SELECTED_LINE=$(echo "$SELECTED_LINES" | shuf -n 1)
FILE=$(echo "$SELECTED_LINE" | sed -E 's/^[[:space:]]*[0-9]+[[:space:]]+//')

if [[ -z "$FILE" || "$FILE" == "$SELECTED_LINE" ]]; then
    echo "Failed to parse file path from: $SELECTED_LINE" >&2
    exit 1
fi

if [[ "$DRY_RUN" -eq 1 ]]; then
    ./scripts/ai-run-roast.sh --dry-run "$FILE"
else
    ./scripts/ai-run-roast.sh "$FILE"
fi
