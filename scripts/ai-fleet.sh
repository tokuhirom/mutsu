#!/usr/bin/env bash
# ai-fleet.sh — Fleet manager for AI agent workers and supervisor.
#
# Maintains a desired fleet composition in tmux windows.
# Monitors liveness, restarts dead workers, and shows status.
#
# Usage:
#   ai-fleet.sh [--codex N] [--claude N] [--supervisor <agent>|--no-supervisor]
#               [--dry-run] [--once] [--interval <seconds>]
#
# Examples:
#   ai-fleet.sh                                  # default: --codex 2 --claude 1 --supervisor codex
#   ai-fleet.sh --codex 3 --claude 2             # 3 codex + 2 claude workers + supervisor(codex)
#   ai-fleet.sh --codex 2 --claude 2 --supervisor claude
#   ai-fleet.sh --codex 1 --no-supervisor        # 1 codex worker, no supervisor
#
# Window naming convention:
#   fleet:codex:1, fleet:codex:2, fleet:claude:1, fleet:supervisor:1
#
# The "fleet:" prefix distinguishes managed windows from manual ones.

set -euo pipefail

DRY_RUN=0
ONCE=0
INTERVAL=30
NUM_CODEX=2
NUM_CLAUDE=1
SUPERVISOR_AGENT="codex"
NO_SUPERVISOR=0
STOP_PATTERN=""

usage() {
    cat <<USAGE
Usage: $0 [options]

Worker options:
  --codex <N>            Number of codex workers (default: 2)
  --claude <N>           Number of claude workers (default: 1)
  --supervisor <agent>   Supervisor agent type: codex or claude (default: codex)
  --no-supervisor        Do not start a supervisor

Actions:
  --stop <pattern>       Gracefully stop workers matching pattern
                         e.g. "fleet:codex:1", "codex" (matches all codex), "all"
                         The fleet manager loop will restart them automatically.

General options:
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
        --codex)
            if [[ $# -lt 2 ]]; then
                echo "Error: --codex requires a value" >&2; exit 1
            fi
            NUM_CODEX="$2"; shift 2 ;;
        --claude)
            if [[ $# -lt 2 ]]; then
                echo "Error: --claude requires a value" >&2; exit 1
            fi
            NUM_CLAUDE="$2"; shift 2 ;;
        --supervisor)
            if [[ $# -lt 2 ]]; then
                echo "Error: --supervisor requires a value" >&2; exit 1
            fi
            SUPERVISOR_AGENT="$2"; shift 2 ;;
        --no-supervisor) NO_SUPERVISOR=1; shift ;;
        --stop)
            if [[ $# -lt 2 ]]; then
                echo "Error: --stop requires a pattern" >&2; exit 1
            fi
            STOP_PATTERN="$2"; shift 2 ;;
        -h|--help)   usage; exit 0 ;;
        *)           echo "Error: unknown argument: $1" >&2; usage; exit 1 ;;
    esac
done

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
STOP_FILE="${REPO_ROOT}/tmp/.stop"
SANDBOX_LOCK_DIR="${REPO_ROOT}/.git/sandbox/.locks"

# --- Build fleet definition from options ---
# Each entry: "window_name|command"
# Window names use "fleet:" prefix for identification.
FLEET=()

for i in $(seq 1 "$NUM_CODEX"); do
    FLEET+=("fleet:codex:${i}|dotenvx run -- ${SCRIPT_DIR}/ai-next-roast.sh --agent codex")
done

for i in $(seq 1 "$NUM_CLAUDE"); do
    FLEET+=("fleet:claude:${i}|dotenvx run -- ${SCRIPT_DIR}/ai-next-roast.sh --agent claude")
done

if [[ "$NO_SUPERVISOR" -eq 0 ]]; then
    FLEET+=("fleet:supervisor:1|dotenvx run -- ${SCRIPT_DIR}/ai-supervisor.sh --agent ${SUPERVISOR_AGENT}")
fi

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
# Find which sandbox branch a worker is using, by matching lock file PIDs
# against the worker's process tree.
find_worker_branch() {
    local pane_pid="$1"
    if [[ ! -d "$SANDBOX_LOCK_DIR" ]]; then
        return
    fi
    # Collect all descendant PIDs (including pane_pid itself)
    local all_pids="$pane_pid"
    local descendants
    descendants=$(get_descendants "$pane_pid")
    if [[ -n "$descendants" ]]; then
        all_pids="$all_pids $descendants"
    fi
    # Check each lock file
    local lock_file lock_pid branch
    for lock_file in "$SANDBOX_LOCK_DIR"/*.pid; do
        [[ -f "$lock_file" ]] || continue
        lock_pid="$(cat "$lock_file" 2>/dev/null || true)"
        if [[ -z "$lock_pid" ]]; then
            continue
        fi
        for pid in $all_pids; do
            if [[ "$pid" == "$lock_pid" ]]; then
                branch="$(basename "$lock_file" .pid)"
                echo "$branch"
                return
            fi
        done
    done
}

# Capture last N lines from a dead pane (for diagnostics)
capture_dead_pane() {
    local name="$1"
    local lines="${2:-10}"
    local idx
    idx=$(window_index "$name")
    if [[ -z "$idx" ]]; then
        return
    fi
    local output
    output=$(tmux capture-pane -t ":$idx" -p -S "-${lines}" 2>/dev/null || true)
    if [[ -n "$output" ]]; then
        echo "  --- Last ${lines} lines from $name ---"
        echo "$output" | sed 's/^/  | /'
        echo "  --- end ---"
    fi
}

launch_window() {
    local name="$1"
    local cmd="$2"
    local idx

    idx=$(window_index "$name")
    if [[ -n "$idx" ]]; then
        # Window exists but pane is dead — capture output, then kill and recreate
        echo "  Killing dead window: $name"
        if [[ "$DRY_RUN" -eq 0 ]]; then
            capture_dead_pane "$name" 20
            tmux kill-window -t ":$idx"
        fi
    fi

    echo "  Launching: $name"
    if [[ "$DRY_RUN" -eq 0 ]]; then
        tmux new-window -n "$name" -d "cd ${REPO_ROOT} && $cmd"
        # Keep the window around when the process exits so we can inspect failures
        tmux set-option -t "$name" remain-on-exit on 2>/dev/null || true
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

        local branch="-"
        if [[ "$status" == "running" && "$pid" != "-" && "$pid" != "?" ]]; then
            branch=$(find_worker_branch "$pid")
            branch="${branch:--}"
        fi

        printf "  %-25s  %-8s  pid=%-8s  branch=%s\n" "$name" "$status" "$pid" "$branch"
        if [[ "$status" == "DEAD" ]]; then
            capture_dead_pane "$name" 5
        fi
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

# Get all descendant PIDs of a given PID
get_descendants() {
    local parent="$1"
    local children
    children=$(pgrep -P "$parent" 2>/dev/null || true)
    for child in $children; do
        echo "$child"
        get_descendants "$child"
    done
}

# Gracefully stop a fleet window by creating stop files for all descendant processes
graceful_stop_window() {
    local name="$1"
    local idx pane_pid descendants

    idx=$(window_index "$name")
    if [[ -z "$idx" ]]; then
        echo "  $name: not found, skipping"
        return 1
    fi

    if ! window_alive "$name"; then
        echo "  $name: already dead"
        return 0
    fi

    pane_pid=$(tmux list-panes -t ":$idx" -F '#{pane_pid}' 2>/dev/null)
    if [[ -z "$pane_pid" ]]; then
        echo "  $name: cannot get pane PID"
        return 1
    fi

    # Create stop files for pane PID and all descendants
    descendants=$(get_descendants "$pane_pid")
    local count=0
    for pid in $pane_pid $descendants; do
        touch "${STOP_FILE}.${pid}"
        count=$((count + 1))
    done
    echo "  $name: sent graceful stop to $count processes (pane_pid=$pane_pid)"
    return 0
}

# Gracefully stop matching fleet windows (create stop files, then exit)
do_stop() {
    local pattern="$1"

    # Collect matching window names
    local matching_names=()
    for entry in "${FLEET[@]}"; do
        local name
        name="$(fleet_window_name "$entry")"
        if [[ "$pattern" == "all" || "$name" == "$pattern" || "$name" == *"$pattern"* ]]; then
            matching_names+=("$name")
        fi
    done

    if [[ ${#matching_names[@]} -eq 0 ]]; then
        echo "No fleet windows match pattern: $pattern"
        echo "Available windows:"
        for entry in "${FLEET[@]}"; do
            echo "  $(fleet_window_name "$entry")"
        done
        exit 1
    fi

    echo "Sending graceful stop to ${#matching_names[@]} window(s):"
    for name in "${matching_names[@]}"; do
        graceful_stop_window "$name"
    done
    echo ""
    echo "Stop files created. Workers will exit after finishing current work."
    echo "The fleet manager loop will restart them automatically."
}

# --- Main ---

# Remove stop file if it exists (we're starting fresh)
if [[ -f "$STOP_FILE" ]]; then
    echo "Removing stale stop file: $STOP_FILE"
    if [[ "$DRY_RUN" -eq 0 ]]; then
        rm -f "$STOP_FILE"
    fi
fi

if [[ -n "$STOP_PATTERN" ]]; then
    do_stop "$STOP_PATTERN"
    exit 0
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
