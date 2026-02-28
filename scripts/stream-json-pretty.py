#!/usr/bin/env python3
"""Filter Claude Code stream-json output into human-readable text.

Usage:
    claude -p --verbose --output-format stream-json ... | stream-json-pretty.py

Claude Code stream-json format (JSONL):
  {"type": "system", ...}           — session init
  {"type": "assistant", "message": {"content": [...]}, ...}  — assistant turn
  {"type": "tool_use", "tool": ..., "input": ...}            — tool call
  {"type": "tool_result", ...}      — tool output
  {"type": "rate_limit_event", ...} — rate limit info
  {"type": "result", ...}           — final result
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

out = sys.stdout


def format_tool_input(tool_input):
    """Format tool input as compact key: value lines."""
    if not isinstance(tool_input, dict):
        s = str(tool_input)
        if len(s) > 200:
            s = s[:200] + "..."
        return f"  {DIM}{s}{RESET}"
    parts = []
    for k, v in tool_input.items():
        sv = str(v)
        if len(sv) > 120:
            sv = sv[:120] + "..."
        parts.append(f"  {DIM}{k}: {sv}{RESET}")
    return "\n".join(parts)


def handle_assistant(data):
    """Handle assistant message — extract text and tool_use blocks."""
    msg = data.get("message", {})
    content = msg.get("content", [])

    print(f"\n{GREEN}--- assistant ---{RESET}", file=out, flush=True)

    for block in content:
        btype = block.get("type")
        if btype == "text":
            text = block.get("text", "")
            if text:
                print(text, file=out, flush=True)
        elif btype == "tool_use":
            name = block.get("name", "?")
            tool_input = block.get("input", {})
            print(f"\n{CYAN}{BOLD}[{name}]{RESET}", file=out, flush=True)
            formatted = format_tool_input(tool_input)
            if formatted:
                print(formatted, file=out, flush=True)


def handle_tool_result(data):
    """Handle tool result."""
    content = data.get("content", "")
    if isinstance(content, list):
        # content can be a list of blocks
        texts = []
        for item in content:
            if isinstance(item, dict):
                texts.append(item.get("text", str(item)))
            else:
                texts.append(str(item))
        content = "\n".join(texts)
    content = str(content)
    if len(content) > 300:
        content = content[:300] + "..."
    if content:
        print(f"  {DIM}{content}{RESET}", file=out, flush=True)


def handle_result(data):
    """Handle the final result JSON."""
    is_error = data.get("is_error", False)
    result_text = data.get("result", "")
    duration = data.get("duration_ms", 0)
    num_turns = data.get("num_turns", 0)
    cost = data.get("total_cost_usd", 0)

    color = RED if is_error else GREEN
    label = "ERROR" if is_error else "DONE"
    print(f"\n{color}{BOLD}=== {label} ==={RESET}", file=out, flush=True)
    if duration:
        secs = duration / 1000
        parts = [f"duration: {secs:.1f}s", f"turns: {num_turns}"]
        if cost:
            parts.append(f"cost: ${cost:.4f}")
        print(f"{DIM}  {', '.join(parts)}{RESET}", file=out, flush=True)
    if is_error and result_text:
        print(f"{RED}  {result_text}{RESET}", file=out, flush=True)
    elif result_text:
        if len(result_text) > 500:
            print(result_text[:500] + "...", file=out, flush=True)
        else:
            print(result_text, file=out, flush=True)


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
        if dtype == "assistant":
            handle_assistant(data)
        elif dtype == "tool_use":
            name = data.get("tool", data.get("name", "?"))
            tool_input = data.get("input", {})
            print(f"\n{CYAN}{BOLD}[{name}]{RESET}", file=out, flush=True)
            formatted = format_tool_input(tool_input)
            if formatted:
                print(formatted, file=out, flush=True)
        elif dtype == "tool_result":
            handle_tool_result(data)
        elif dtype == "result":
            handle_result(data)
        elif dtype == "system":
            model = data.get("model", "")
            print(f"{DIM}[session: model={model}]{RESET}", file=out, flush=True)
        # rate_limit_event and others: skip silently


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        sys.exit(130)
    except BrokenPipeError:
        sys.exit(0)
