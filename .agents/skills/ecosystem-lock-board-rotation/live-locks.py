#!/usr/bin/env python3
"""live-locks.py — compute the live locks on an ecosystem lock board from its comment log.

    .agents/skills/ecosystem-lock-board-rotation/live-locks.py tmp/board-*.json [--check-origin]

Reads one or more files holding the board issue's comments as JSON — `gh api
--paginate` output (concatenated arrays), a saved `issue_read get_comments`
page, or anything else that is an array (or several arrays) of objects with
`id`, `body` and `created_at`. Pages may overlap; comments are de-duplicated by
id and replayed in id order, which is the board's own order.

A `Locking: <dist> <branch>` line is live until an `Unlocking: <dist> <branch>`
line with the same distribution *and* the same branch follows it. That is the
whole rule, and it is applied here mechanically, because the #8977 -> #10045
rotation carried six locks that had been released days earlier: the live set
was assembled from memory and a summary instead of from the log.

Also reported, because a rotation has to decide about each by hand:

- `malformed`: a first line that looks like a lock operation but is not one
  (`Releasing:` on the board, a missing branch). It releases nothing.
- `orphan unlock`: an `Unlocking:` with no live lock to match — usually a typo
  in the distribution or branch, which can leave the real lock live.

With `--check-origin`, each live lock is tested against the stale rule: older
than 24 hours *and* its branch absent from `origin` (`git ls-remote`). A merged
or closed PR also satisfies the rule, but that needs a GitHub lookup this
script does not do; check it yourself for any lock whose branch still exists.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import re
import subprocess
import sys

LOCK_RE = re.compile(r"^(Locking|Unlocking):\s+(\S+)(?:\s+(\S+))?(.*)$")
SUSPECT_RE = re.compile(r"^\s*(Lock|Unlock|Releas|Claim)\w*:", re.IGNORECASE)
STALE_AGE = dt.timedelta(hours=24)


def load_comments(paths: list[str]) -> list[dict]:
    """Every comment object in the given files, de-duplicated by id, in id order."""
    seen: dict[int, dict] = {}
    decoder = json.JSONDecoder()
    for path in paths:
        text = sys.stdin.read() if path == "-" else open(path, encoding="utf-8").read()
        pos = 0
        while True:
            while pos < len(text) and text[pos].isspace():
                pos += 1
            if pos >= len(text):
                break
            value, pos = decoder.raw_decode(text, pos)
            stack = [value]
            while stack:
                item = stack.pop()
                if isinstance(item, list):
                    stack.extend(item)
                elif isinstance(item, dict) and "id" in item and "body" in item:
                    seen[int(item["id"])] = item
    return [seen[k] for k in sorted(seen)]


def parse_time(stamp: str) -> dt.datetime:
    return dt.datetime.fromisoformat(stamp.replace("Z", "+00:00"))


def branch_on_origin(branch: str) -> bool:
    out = subprocess.run(
        ["git", "ls-remote", "--heads", "origin", branch],
        capture_output=True, text=True, check=True,
    ).stdout
    return bool(out.strip())


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("files", nargs="+", help="comment JSON files ('-' for stdin)")
    ap.add_argument("--check-origin", action="store_true",
                    help="apply the stale rule (24h old and no origin branch) to each live lock")
    ap.add_argument("--now", help="reference time for --check-origin (ISO 8601, default: now)")
    args = ap.parse_args()

    comments = load_comments(args.files)
    live: dict[tuple[str, str], dict] = {}
    malformed: list[tuple[dict, str]] = []
    orphans: list[tuple[dict, str]] = []

    for c in comments:
        first = (c.get("body") or "").splitlines()[0:1]
        first = first[0].strip() if first else ""
        m = LOCK_RE.match(first)
        if not m or not m.group(3):
            if SUSPECT_RE.match(first):
                malformed.append((c, first))
            continue
        op, dist, branch = m.group(1), m.group(2), m.group(3)
        key = (dist, branch)
        if op == "Locking":
            live.setdefault(key, c)
        elif key in live:
            del live[key]
        else:
            orphans.append((c, first))

    now = parse_time(args.now) if args.now else dt.datetime.now(dt.timezone.utc)
    print(f"{len(comments)} comments read, ids {comments[0]['id'] if comments else '-'}"
          f" .. {comments[-1]['id'] if comments else '-'}")
    print(f"\nlive locks: {len(live)}")
    for (dist, branch), c in sorted(live.items(), key=lambda kv: int(kv[1]["id"])):
        line = f"  {c['id']}  {c['created_at']}  {dist}  {branch}"
        if args.check_origin:
            age = now - parse_time(c["created_at"])
            on_origin = branch_on_origin(branch)
            hours = int(age.total_seconds() // 3600)
            if age > STALE_AGE and not on_origin:
                line += f"  STALE (no origin branch after {hours}h)"
            else:
                line += f"  held ({hours}h, origin branch {'present' if on_origin else 'absent'})"
        print(line)
    if malformed:
        print(f"\nmalformed lock lines (release nothing): {len(malformed)}")
        for c, first in malformed:
            print(f"  {c['id']}  {c['created_at']}  {first}")
    if orphans:
        print(f"\norphan unlocks (no live lock matched): {len(orphans)}")
        for c, first in orphans:
            print(f"  {c['id']}  {c['created_at']}  {first}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
