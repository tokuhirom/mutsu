#!/usr/bin/env python3
"""PostToolUse hook: flag a `t/**.t` file written to the wrong directory.

A test's directory under `t/` is a function of its basename
(`scripts/migrate-t-layout.py`, docs/t-directory-layout.md), so choosing it by
what the test is "about" goes wrong whenever the name says otherwise -- a file
named `method-...` belongs in `t/oo/method/`, not `t/oo/`. `make checks`
catches that, but only at gate time, and lefthook's pre-commit does not run in
a remote container at all. This runs the same placement rule the moment the
file is written and tells the agent where it belongs.

Exit 2 sends stderr back to the agent; anything else is silent.
"""

import json
import os
import subprocess
import sys


def main() -> int:
    try:
        payload = json.load(sys.stdin)
    except ValueError:
        return 0
    path = (payload.get("tool_input") or {}).get("file_path") or ""
    root = os.environ.get("CLAUDE_PROJECT_DIR") or os.getcwd()
    if not path.endswith(".t"):
        return 0
    rel = os.path.relpath(os.path.abspath(path), root)
    if not rel.startswith("t" + os.sep):
        return 0
    result = subprocess.run(
        [sys.executable, os.path.join(root, "scripts", "migrate-t-layout.py"), "--where", rel],
        cwd=root,
        capture_output=True,
        text=True,
    )
    if result.returncode == 0:
        return 0
    print(
        f"t/ layout: {rel} is misplaced (make check-t-layout will fail).\n"
        f"Rules say: {result.stdout.strip() or '(no path)'}\n{result.stderr.rstrip()}\n"
        "Move it with `git mv`/`mv` (see docs/t-directory-layout.md).",
        file=sys.stderr,
    )
    return 2


if __name__ == "__main__":
    sys.exit(main())
