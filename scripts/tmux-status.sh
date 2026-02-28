#!/usr/bin/env bash
# Show status of all tmux agent windows (pid, alive/dead, last few lines)
set -euo pipefail

LINES="${1:-5}"

tmux list-windows -F '#{window_index} #{window_name}' | while read -r idx name; do
    pid=$(tmux list-panes -t ":$idx" -F '#{pane_pid}' 2>/dev/null || echo "?")
    dead=$(tmux list-panes -t ":$idx" -F '#{pane_dead}' 2>/dev/null || echo "?")
    if [[ "$dead" == "1" ]]; then
        state="DEAD"
    else
        state="alive"
    fi
    echo "=== [$idx] $name (pid=$pid $state) ==="
    tmux capture-pane -t ":$idx" -p -S "-${LINES}" 2>/dev/null || echo "(capture failed)"
    echo
done
