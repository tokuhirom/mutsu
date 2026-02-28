#!/usr/bin/env bash
# ai-fleet.sh — Fleet manager for AI agent workers and supervisor.
#
# Maintains a desired fleet composition in tmux windows.
# Monitors liveness, restarts dead workers, and shows status.
#
# Usage:
#   ai-fleet.sh [--dry-run] [--once] [--interval <seconds>]
#
# Fleet composition (edit FLEET array below to change):
#   codex worker x2, claude worker x1, supervisor x1
#
# Window naming convention:
#   fleet:codex:1, fleet:codex:2, fleet:claude:1, fleet:supervisor:1
#
# The "fleet:" prefix distinguishes managed windows from manual ones.

set -euo pipefail

DRY_RUN=0
ONCE=0
INTERVAL=30

usage() {
    cat <<USAGE
Usage: $0 [--dry-run] [--once] [--interval <seconds>]

Options:
  --dry-run              Show what would be done without executing
  --once                 Check once and exit (don't loop)
  --interval <seconds>   Poll interval (default: 30)
  -h, --help             Show this help
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run)   DRY_RUN=1; shift ;;
        --once)      ONCE=1; shift ;;
        --interval)
            if [[ $# -lt 2 ]]; then
                echo "Error: --interval requires a value" >&2; exit 1
            fi
            INTERVAL="$2"; shift 2 ;;
        -h|--help)   usage; exit 0 ;;
        *)           echo "Error: unknown argument: $1" >&2; usage; exit 1 ;;
    esac
done

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
STOP_FILE="${REPO_ROOT}/tmp/.stop"

# --- Fleet definition ---
# Each entry: "window_name|command"
# Window names use "fleet:" prefix for identification.
FLEET=(
    "fleet:codex:1|dotenvx run -- ${SCRIPT_DIR}/ai-next-roast.sh --agent codex"
    "fleet:codex:2|dotenvx run -- ${SCRIPT_DIR}/ai-next-roast.sh --agent codex"
    "fleet:claude:1|dotenvx run -- ${SCRIPT_DIR}/ai-next-roast.sh --agent claude"
    "fleet:supervisor:1|dotenvx run -- ${SCRIPT_DIR}/ai-supervisor.sh --agent codex"
)

# Check if running inside tmux
if [[ -z "${TMUX:-}" ]]; then
    echo "Error: must be run inside a tmux session" >&2
    exit 1
fi

# Get the window name for a fleet entry
fleet_window_name() {
    echo "${1%%|*}"
}

# Get the command for a fleet entry
fleet_command() {
    echo "${1#*|}"
}

# Check if a tmux window exists and its pane is alive
# Returns: 0=alive, 1=dead/missing
window_alive() {
    local name="$1"
    local dead
    dead=$(tmux list-windows -F '#{window_name} #{pane_dead}' 2>/dev/null \
        | awk -v n="$name" '$1 == n { print $2 }')
    if [[ -z "$dead" ]]; then
        return 1  # window doesn't exist
    fi
    if [[ "$dead" == "1" ]]; then
        return 1  # pane is dead
    fi
    return 0
}

# Get window index by name (empty if not found)
window_index() {
    local name="$1"
    tmux list-windows -F '#{window_name} #{window_index}' 2>/dev/null \
        | awk -v n="$name" '$1 == n { print $2 }'
}

# Launch a fleet window
launch_window() {
    local name="$1"
    local cmd="$2"
    local idx

    idx=$(window_index "$name")
    if [[ -n "$idx" ]]; then
        # Window exists but pane is dead — kill and recreate
        echo "  Killing dead window: $name"
        if [[ "$DRY_RUN" -eq 0 ]]; then
            tmux kill-window -t ":$idx"
        fi
    fi

    echo "  Launching: $name"
    if [[ "$DRY_RUN" -eq 0 ]]; then
        tmux new-window -n "$name" -d "cd ${REPO_ROOT} && $cmd"
    fi
}

# Show status of all fleet windows
show_status() {
    local ts
    ts="$(date '+%H:%M:%S')"
    echo "[$ts] Fleet status:"

    for entry in "${FLEET[@]}"; do
        local name cmd status pid
        name="$(fleet_window_name "$entry")"
        cmd="$(fleet_command "$entry")"
        local idx
        idx=$(window_index "$name")

        if [[ -z "$idx" ]]; then
            status="MISSING"
            pid="-"
        else
            pid=$(tmux list-panes -t ":$idx" -F '#{pane_pid}' 2>/dev/null || echo "?")
            if window_alive "$name"; then
                status="running"
            else
                status="DEAD"
            fi
        fi

        printf "  %-25s  %-8s  pid=%-8s\n" "$name" "$status" "$pid"
    done
}

# Ensure all fleet members are running
reconcile() {
    local any_changed=0

    for entry in "${FLEET[@]}"; do
        local name cmd
        name="$(fleet_window_name "$entry")"
        cmd="$(fleet_command "$entry")"

        if ! window_alive "$name"; then
            launch_window "$name" "$cmd"
            any_changed=1
        fi
    done

    if [[ "$any_changed" -eq 0 ]]; then
        echo "  All fleet members running."
    fi
}

# --- Main ---

# Remove stop file if it exists (we're starting fresh)
if [[ -f "$STOP_FILE" ]]; then
    echo "Removing stale stop file: $STOP_FILE"
    if [[ "$DRY_RUN" -eq 0 ]]; then
        rm -f "$STOP_FILE"
    fi
fi

if [[ "$ONCE" -eq 1 ]]; then
    show_status
    echo ""
    reconcile
    exit 0
fi

echo "Fleet manager started (interval=${INTERVAL}s, Ctrl-C to stop)"
echo ""

while true; do
    if [[ -f "$STOP_FILE" ]]; then
        echo "Stop file detected. Fleet manager exiting."
        break
    fi

    show_status
    echo ""
    reconcile
    echo ""
    sleep "$INTERVAL"
done
