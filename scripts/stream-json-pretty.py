#!/usr/bin/env python3
"""Filter Claude Code stream-json output into human-readable text.

Usage:
    claude -p --output-format stream-json ... | stream-json-pretty.py
"""

import json
import sys

RESET = "\033[0m"
DIM = "\033[2m"
CYAN = "\033[36m"
YELLOW = "\033[33m"
GREEN = "\033[32m"
RED = "\033[31m"
BOLD = "\033[1m"

# All output goes to stderr so it is visible even when stdout is redirected.
# text_delta (the assistant's prose) also goes to stderr for consistency;
# only the final "result" text goes to stdout so callers can capture it.
out = sys.stderr

current_tool = None
tool_input_buf = ""


def flush_tool_input():
    global tool_input_buf
    if tool_input_buf:
        try:
            parsed = json.loads(tool_input_buf)
            # Show a compact summary of tool parameters
            parts = []
            for k, v in parsed.items():
                sv = str(v)
                if len(sv) > 120:
                    sv = sv[:120] + "..."
                parts.append(f"  {DIM}{k}: {sv}{RESET}")
            if parts:
                print("\n".join(parts), file=out, flush=True)
        except json.JSONDecodeError:
            if len(tool_input_buf) > 200:
                print(f"  {DIM}{tool_input_buf[:200]}...{RESET}", file=out, flush=True)
            else:
                print(f"  {DIM}{tool_input_buf}{RESET}", file=out, flush=True)
        tool_input_buf = ""


def handle_event(event):
    global current_tool, tool_input_buf

    etype = event.get("type")

    if etype == "content_block_start":
        block = event.get("content_block", {})
        if block.get("type") == "tool_use":
            flush_tool_input()
            current_tool = block.get("name", "?")
            print(f"\n{CYAN}{BOLD}[{current_tool}]{RESET}", file=out, flush=True)
            tool_input_buf = ""
        elif block.get("type") == "text":
            pass  # text block starting

    elif etype == "content_block_delta":
        delta = event.get("delta", {})
        dtype = delta.get("type")
        if dtype == "text_delta":
            flush_tool_input()
            text = delta.get("text", "")
            print(text, end="", file=out, flush=True)
        elif dtype == "input_json_delta":
            tool_input_buf += delta.get("partial_json", "")

    elif etype == "content_block_stop":
        flush_tool_input()

    elif etype == "message_start":
        msg = event.get("message", {})
        role = msg.get("role", "")
        if role == "assistant":
            print(f"\n{GREEN}--- assistant ---{RESET}", file=out, flush=True)

    elif etype == "message_delta":
        delta = event.get("delta", {})
        stop = delta.get("stop_reason")
        usage = event.get("usage", {})
        if stop:
            parts = [f"stop={stop}"]
            if usage:
                out_tokens = usage.get("output_tokens")
                if out_tokens:
                    parts.append(f"tokens={out_tokens}")
            print(f"\n{DIM}[{', '.join(parts)}]{RESET}", file=out, flush=True)

    elif etype == "message_stop":
        pass


def handle_result(data):
    """Handle the final result JSON (non-stream_event)."""
    is_error = data.get("is_error", False)
    result_text = data.get("result", "")
    duration = data.get("duration_ms", 0)
    num_turns = data.get("num_turns", 0)

    color = RED if is_error else GREEN
    label = "ERROR" if is_error else "DONE"
    print(f"\n{color}{BOLD}=== {label} ==={RESET}", file=out, flush=True)
    if duration:
        secs = duration / 1000
        print(f"{DIM}  duration: {secs:.1f}s, turns: {num_turns}{RESET}", file=out, flush=True)
    if is_error and result_text:
        print(f"{RED}  {result_text}{RESET}", file=out, flush=True)
    elif result_text:
        # Print final result text (truncated if huge)
        if len(result_text) > 500:
            print(result_text[:500] + "...", flush=True)
        else:
            print(result_text, flush=True)


def main():
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            data = json.loads(line)
        except json.JSONDecodeError:
            print(f"{DIM}{line}{RESET}", file=out, flush=True)
            continue

        dtype = data.get("type")
        if dtype == "stream_event":
            event = data.get("event", {})
            handle_event(event)
        elif dtype == "result":
            handle_result(data)
        # other types: pass through silently


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        sys.exit(130)
    except BrokenPipeError:
        sys.exit(0)
