#!/usr/bin/env python3
"""ecosystem-dashboard.py -- render the ecosystem work queue as one issue body.

A Renovate-style "dashboard": the failing distributions, who holds a lock on
which, and which open issue each one waits on, as a bulleted list. It is a
READ-ONLY VIEW. The sources of truth stay where they are:

- the queue is `scripts/ecosystem-tickets.py --queue` (computed from the ledger);
- a lock is a `Locking:` / `Unlocking:` comment on the lock board (the issue
  labelled `ecosystem:lock`; the protocol is the ecosystem-dist-queue skill);
- a link from a distribution to an issue is a MARKER LINE in the issue's body:

      eco-dist: String::Utils, Foo::Bar      (names as the ledger spells them)
      eco-cluster: 3f0a1c2d                  (every dist of that cluster; the line
                                              `ecosystem-tickets.py --issue` emits)

Nothing here writes to the ledger or to the lock board. The dashboard issue's
BODY is replaced wholesale on each run, so two runs cannot lose each other's
edits the way a hand-edited body would; locks are still taken and released by
appending comments to the board.

    scripts/ecosystem-dashboard.py                    # print the body (needs `gh api`)
    scripts/ecosystem-dashboard.py --update 15500     # replace that issue's body
    scripts/ecosystem-dashboard.py --issues-json tmp/issues.json --comments-json tmp/board.json
    scripts/ecosystem-dashboard.py --self-test

`--issues-json` / `--comments-json` take the saved REST output (`gh api --paginate`
or a saved `issue_read` page) for a session that cannot call `gh`.

Besides the three lists the body has a "Needs attention" part: an open
`ecosystem` issue with no marker that names a queued distribution (so the link
is missing), and a marker that names a distribution no longer in the queue.
"""

from __future__ import annotations

import argparse
import datetime as dt
import importlib.util
import json
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(HERE)
SLUG = "tokuhirom/mutsu"
DISTS_DIR = os.path.join(REPO, "ecosystem", "dists")
LIVE_LOCKS = os.path.join(REPO, ".agents", "skills", "ecosystem-lock-board-rotation",
                          "live-locks.py")

sys.path.insert(0, HERE)
import ecosystem_common as eco  # noqa: E402

DIST_MARKER = re.compile(r"eco-dist:[ \t]*([^\n`]+)")
CLUSTER_MARKER = re.compile(r"eco-cluster:[ \t]*([0-9a-f]{6,})")
JST = dt.timezone(dt.timedelta(hours=9))


def _load(path: str, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    sys.modules[name] = mod
    spec.loader.exec_module(mod)
    return mod


def markers(body: str) -> tuple[list[str], list[str]]:
    """The distributions and cluster ids an issue body names, in order."""
    dists: list[str] = []
    for m in DIST_MARKER.finditer(body or ""):
        for part in m.group(1).split(","):
            name = part.strip().strip("`*").strip()
            if name and name not in dists:
                dists.append(name)
    return dists, list(dict.fromkeys(CLUSTER_MARKER.findall(body or "")))


def mentions(dist: str, text: str) -> bool:
    """`dist` as a whole name in free text (not as part of a longer name)."""
    return re.search(r"(?<![\w:])" + re.escape(dist) + r"(?![\w:-])", text or "") is not None


def issue_links(issues, cluster_dists) -> dict[str, list[int]]:
    """dist -> numbers of the open issues whose marker names it, ascending."""
    out: dict[str, set[int]] = {}
    for it in issues:
        dists, clusters = markers(it.get("body") or "")
        for c in clusters:
            dists += [d for d in cluster_dists.get(c, ()) if d not in dists]
        for d in dists:
            out.setdefault(d, set()).add(it["number"])
    return {d: sorted(ns) for d, ns in out.items()}


SAFE_NAME = re.compile(r"[A-Za-z0-9][A-Za-z0-9_.:/@+-]*")


def live_locks(comments, live_locks_mod) -> list[tuple[str, str, int]]:
    """(dist, branch, comment id) of every lock still live, oldest first -- the
    same replay `live-locks.py` does for a rotation, so there is one rule."""
    live: dict[tuple[str, str], int] = {}
    for c in sorted(comments, key=lambda c: int(c["id"])):
        first = (c.get("body") or "").splitlines()[0:1]
        m = live_locks_mod.LOCK_RE.match(first[0].strip() if first else "")
        if not m or not m.group(3):
            continue
        key = (m.group(2), m.group(3))
        # Board comments are written by anyone who can comment, and this ends up
        # in an issue body that an unattended job rewrites: keep only names that
        # cannot carry markup, links or mentions.
        if not all(SAFE_NAME.fullmatch(k) for k in key):
            continue
        if m.group(1) == "Locking":
            live.setdefault(key, int(c["id"]))
        else:
            live.pop(key, None)
    return [(d, b, i) for (d, b), i in sorted(live.items(), key=lambda kv: kv[1])]


def refs(numbers) -> str:
    return ", ".join(f"#{n}" for n in numbers)


def render(rows, locks, issues, cluster_dists, *, now, board=None) -> str:
    """The dashboard body. `rows` are `ecosystem-tickets.py` queue rows."""
    links = issue_links(issues, cluster_dists)
    queued = {r["dist"] for r in rows}
    locked = {d: b for d, b, _ in locks}

    held = [(d, b) for d, b, _ in locks]
    waiting = [r for r in rows if r["dist"] not in locked and r["dist"] in links]
    free = [r for r in rows if r["dist"] not in locked and r["dist"] not in links]

    out = [
        "This is a generated view; the body is rewritten by "
        "`scripts/ecosystem-dashboard.py`, so edits here are lost. "
        "Locks are taken on the "
        + (f"[lock board](https://github.com/{SLUG}/issues/{board})" if board
           else "lock board")
        + "; a distribution is linked to an issue by an `eco-dist: <Dist>` line in "
        "that issue's body (see the `ecosystem-dist-queue` skill).",
        "",
        f"_Updated {now.astimezone(JST):%Y-%m-%d %H:%M} JST._ "
        f"**{len(rows)}** queued: {len(held)} locked, {len(waiting)} waiting on an "
        f"issue, {len(free)} free.",
        "",
        "## Locked", "",
    ]
    out += [f"- {d} — `{b}`" + (f" — {refs(links[d])}" if d in links else "")
            for d, b in held] or ["_none_"]
    out += ["", "## Waiting on an issue", ""]
    out += [f"- {r['dist']} — {refs(links[r['dist']])}" for r in waiting] or ["_none_"]
    out += ["", f"<details><summary>Free queue ({len(free)}, work order)</summary>", ""]
    out += [f"{i}. {r['dist']}" for i, r in enumerate(free, 1)] or ["_empty_"]
    out += ["", "</details>", "", "## Needs attention", ""]

    attention = []
    for it in issues:
        dists, clusters = markers(it.get("body") or "")
        if dists or clusters:
            for d in dists:
                if d not in queued:
                    attention.append(f"- #{it['number']} names `{d}`, which is not in the "
                                     "queue (fixed, excluded, or misspelled)")
            continue
        text = (it.get("title") or "") + "\n" + (it.get("body") or "")
        hits = [r["dist"] for r in rows if mentions(r["dist"], text)]
        if hits:
            attention.append(f"- #{it['number']} mentions {', '.join(f'`{h}`' for h in hits)}"
                             " but has no `eco-dist:` marker")
    out += attention or ["_nothing_"]
    return "\n".join(out) + "\n"


def gh_json(path: str, *params: str, per_page: int = 100):
    """Every item of a list endpoint, page by page. `--paginate` is not used: it
    follows the `Link` header to `repositories/{id}/...`, which the session proxy
    refuses (and a numeric path is not something to depend on anyway)."""
    items: list[dict] = []
    page = 1
    while True:
        cmd = ["gh", "api", "-X", "GET", path, "-f", f"per_page={per_page}", "-f", f"page={page}"]
        for p in params:
            cmd += ["-f", p]
        got = flatten(subprocess.run(cmd, capture_output=True, text=True, check=True).stdout)
        items += got
        if len(got) < per_page:
            return items
        page += 1


def flatten(text: str) -> list[dict]:
    """Concatenated JSON arrays (what `gh api --paginate` prints) as one list."""
    dec, pos, items = json.JSONDecoder(), 0, []
    while True:
        while pos < len(text) and text[pos].isspace():
            pos += 1
        if pos >= len(text):
            return items
        value, pos = dec.raw_decode(text, pos)
        items += value if isinstance(value, list) else [value]


def read_file(path: str) -> list[dict]:
    with open(path, encoding="utf-8") as fh:
        return flatten(fh.read())


def self_test() -> int:
    fails = 0

    def check(name, got, want):
        nonlocal fails
        if got != want:
            print(f"FAIL {name}: got {got!r}, want {want!r}")
            fails += 1

    check("dist marker", markers("x\n`eco-dist: A::B, C, A::B`\n"), (["A::B", "C"], []))
    check("cluster marker", markers("`eco-cluster: 3f0a1c2d`  ·  family `x`"),
          ([], ["3f0a1c2d"]))
    check("no marker", markers("plain"), ([], []))
    check("mention whole", mentions("A::B", "fails in A::B here"), True)
    check("mention longer", mentions("A::B", "fails in A::B::C here"), False)
    check("mention prefix", mentions("JSON", "JSON::Fast breaks"), False)

    mod = _load(LIVE_LOCKS, "live_locks")
    cm = [{"id": 1, "body": "Locking: A x/1"}, {"id": 2, "body": "Locking: B x/2"},
          {"id": 3, "body": "Unlocking: A x/1\nMerged"}, {"id": 4, "body": "Locking: A x/3"}]
    check("live locks", live_locks(cm, mod), [("B", "x/2", 2), ("A", "x/3", 4)])
    check("unsafe lock ignored",
          live_locks([{"id": 9, "body": "Locking: A `x@evil`"}], mod), [])

    rows = [{"dist": d} for d in ("A", "B", "C", "D")]
    issues = [{"number": 10, "title": "t", "body": "eco-dist: B"},
              {"number": 11, "title": "t", "body": "eco-cluster: abcdef12"},
              {"number": 12, "title": "C is slow", "body": ""},
              {"number": 13, "title": "t", "body": "eco-dist: Gone"}]
    body = render(rows, [("A", "x/3", 4)], issues, {"abcdef12": ["D", "B"]},
                  now=dt.datetime(2026, 10, 29, tzinfo=dt.timezone.utc))
    for want in ("- A — `x/3`", "- B — #10, #11", "- D — #11", "1. C",
                 "#12 mentions `C` but has no `eco-dist:` marker",
                 "#13 names `Gone`, which is not in the queue"):
        check("render has " + want, want in body, True)
    check("render counts", "**4** queued: 1 locked, 2 waiting on an issue, 1 free" in body,
          True)
    print(f"ecosystem-dashboard self-test: {fails} failed")
    return 1 if fails else 0


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--issues-json", metavar="FILE",
                    help="open `ecosystem` issues, saved REST output (default: ask `gh api`)")
    ap.add_argument("--comments-json", metavar="FILE",
                    help="the lock board's comments, saved REST output (default: `gh api`)")
    ap.add_argument("--board", type=int, metavar="N",
                    help="lock board issue number (default: the open `ecosystem:lock` issue)")
    ap.add_argument("--update", type=int, metavar="N",
                    help="replace the body of issue N with the dashboard")
    ap.add_argument("--self-test", action="store_true")
    args = ap.parse_args()
    if args.self_test:
        return self_test()

    tickets = _load(os.path.join(HERE, "ecosystem-tickets.py"), "ecosystem_tickets")
    live_mod = _load(LIVE_LOCKS, "live_locks")
    eco.require_ledger(DISTS_DIR)
    records = eco.load_records(DISTS_DIR)
    clusters = tickets.cluster(records)
    rows, _ = tickets.queue(records, exclude=eco.load_exclude(tickets.EXCLUDE_LIST),
                            accepted=eco.load_accepted(tickets.ACCEPTED_LIST),
                            clusters=clusters)
    cluster_dists = {c["id"]: c["dists"] for c in clusters}

    issues = (read_file(args.issues_json) if args.issues_json else
              gh_json(f"repos/{SLUG}/issues", "labels=ecosystem", "state=open"))
    issues = [i for i in issues if "pull_request" not in i
              and not any(l["name"] == "ecosystem:lock" for l in i.get("labels") or [])]

    board = args.board
    if args.comments_json:
        comments = read_file(args.comments_json)
    else:
        if board is None:
            found = gh_json(f"repos/{SLUG}/issues", "labels=ecosystem:lock", "state=open")
            if len(found) != 1:
                raise SystemExit(f"expected exactly one open ecosystem:lock issue, "
                                 f"found {len(found)}")
            board = found[0]["number"]
        comments = gh_json(f"repos/{SLUG}/issues/{board}/comments")

    body = render(rows, live_locks(comments, live_mod), issues, cluster_dists,
                  now=dt.datetime.now(dt.timezone.utc), board=board)
    if args.update:
        subprocess.run(["gh", "api", "-X", "PATCH", f"repos/{SLUG}/issues/{args.update}",
                        "-f", f"body={body}"], check=True, capture_output=True)
        print(f"updated #{args.update}")
    else:
        sys.stdout.write(body)
    return 0


if __name__ == "__main__":
    sys.exit(main())
