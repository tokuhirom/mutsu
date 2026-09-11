#!/usr/bin/env python3
"""ecosystem-ci.py — the two decisions .github/workflows/ecosystem-sweep.yml has to make.

Both live here rather than inline in the workflow so they can be exercised
locally (`--self-test`) instead of being debugged one dispatched run at a time.

    scripts/ecosystem-ci.py plan --scope all --shards letters
    scripts/ecosystem-ci.py plan --scope only --only 'BTree, Trie'
    scripts/ecosystem-ci.py provenance ecosystem/dists/B/BTree.json …
    scripts/ecosystem-ci.py --self-test

`plan` turns the workflow's dispatch inputs into the job matrix: a list of
`{id, args}` chunks, each `args` a validated `ecosystem-sweep.py` selector. It
validates every input here, at plan time, so a typo fails in seconds instead of
after a build and a rakudo install — and so the workflow can word-split
`args` without smuggling anything into a shell.

`provenance` answers the question docs/ecosystem-parity.md section 8 makes the
operator ask before landing a sweep: was all of this measured by ONE mutsu
commit, on ONE rakudo, on ONE host? Records each carry their own provenance, so
a mixed set is still honest data and still lands; what it must not do is become
a `history.tsv` row, because that row reports a single triple for the whole
sweep. So this prints a table and `uniform=true|false`, and the workflow uses
that to decide whether `--rollup` may append history.

Anything written to GITHUB_OUTPUT is also printed, so a local run shows exactly
what the workflow would consume.
"""

from __future__ import annotations

import argparse
import collections
import json
import os
import re
import sys

# One shard per `ecosystem/dists/` directory: A-Z plus `_` for a distribution
# whose name does not start with an ASCII letter (`shard_of()` in
# ecosystem-sweep.py). The whole corpus is exactly these 27.
SHARDS = [chr(c) for c in range(ord("A"), ord("Z") + 1)] + ["_"]

# `status` values a record can carry; see ecosystem/README.md.
STATUSES = {"green", "partial", "red", "no_baseline", "blocked_load",
            "blocked_dep", "skipped"}

DIST_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9:_.+-]*$")


class PlanError(Exception):
    pass


def emit(name, value):
    """Set a workflow output, and echo it so a local run is readable."""
    print(f"{name}={value}")
    path = os.environ.get("GITHUB_OUTPUT")
    if path:
        with open(path, "a", encoding="utf-8") as fh:
            fh.write(f"{name}={value}\n")


# --- plan --------------------------------------------------------------------

def parse_only(raw):
    names = [n for n in re.split(r"[,\s]+", (raw or "").strip()) if n]
    if not names:
        raise PlanError("scope=only needs at least one distribution in `only`")
    bad = [n for n in names if not DIST_RE.match(n)]
    if bad:
        raise PlanError(f"not distribution names: {', '.join(bad)}")
    return names


def parse_prefix(raw):
    value = (raw or "").strip()
    if len(value) != 1 or not (value.isascii() and (value.isalpha() or value == "_")):
        raise PlanError(f"prefix must be one letter A-Z or `_`, got {value!r}")
    return value.upper()


def parse_status(raw):
    value = (raw or "").strip()
    if value not in STATUSES:
        raise PlanError(f"status must be one of {', '.join(sorted(STATUSES))}, got {value!r}")
    return value


def plan(scope, *, prefix="", only="", status="", shards="auto"):
    """The dispatch inputs -> the job matrix. Returns a list of {id, args}."""
    if shards not in ("auto", "letters", "single"):
        raise PlanError(f"shards must be auto/letters/single, got {shards!r}")

    if scope == "only":
        # `--only` ignores `--prefix` in the harness, so this is always one job.
        args = " ".join(f"--only {n}" for n in parse_only(only))
        return [{"id": "only", "args": args}]

    if scope == "prefix":
        letter = parse_prefix(prefix)
        return [{"id": letter, "args": f"--prefix {letter}"}]

    if scope == "all":
        extra, single_id = "--all", "all"
    elif scope == "stale":
        extra, single_id = "--stale", "stale"
    elif scope == "status":
        value = parse_status(status)
        extra, single_id = f"--status {value}", f"status-{value}"
    else:
        raise PlanError(f"unknown scope {scope!r}")

    # `auto` fans out only for the whole corpus: that is the run measured in
    # hours, and it is the one the 6-hour job ceiling actually threatens. A
    # `--stale` / `--status` run is proportional to how much of the ledger is
    # already measured, so paying 27 setups (a build download, a rakudo install)
    # to re-measure a handful of distributions costs more than it saves --
    # until the ledger is large, at which point `shards: letters` says so
    # explicitly.
    fan_out = shards == "letters" or (shards == "auto" and scope == "all")
    if not fan_out:
        # `--all` is implied by `--prefix`, and passing both is redundant; every
        # other scope keeps its selector as-is.
        return [{"id": single_id, "args": extra}]
    # The harness composes selectors: `--prefix S --stale` is the stale subset
    # of one shard. `--all` needs no companion once a prefix narrows it.
    if scope == "all":
        return [{"id": s, "args": f"--prefix {s}"} for s in SHARDS]
    return [{"id": s, "args": f"--prefix {s} {extra}"} for s in SHARDS]


def cmd_plan(args):
    try:
        chunks = plan(args.scope, prefix=args.prefix, only=args.only,
                      status=args.status, shards=args.shards)
    except PlanError as exc:
        print(f"::error title=ecosystem-sweep plan::{exc}", file=sys.stderr)
        return 1
    emit("chunks", json.dumps(chunks, separators=(",", ":")))
    emit("count", len(chunks))
    # A full-corpus run is the only one whose numbers may become a history row
    # (a partial sweep's rollup is not comparable with the rows around it).
    emit("full-corpus", "true" if args.scope == "all" else "false")
    for chunk in chunks:
        print(f"  {chunk['id']:12} {chunk['args']}", file=sys.stderr)
    return 0


# --- provenance --------------------------------------------------------------

def provenance(paths):
    """Group records by their measurement provenance. Returns (triples, unreadable)."""
    triples = collections.Counter()
    unreadable = []
    for path in paths:
        try:
            with open(path, encoding="utf-8") as fh:
                measured = json.load(fh)["measured"]
            triples[(measured["mutsu_commit"], measured["raku_version"],
                     measured.get("host", "?"))] += 1
        except (OSError, ValueError, KeyError) as exc:
            unreadable.append(f"{path}: {exc}")
    return triples, unreadable


def cmd_provenance(args):
    paths = list(args.paths)
    if not paths:
        paths = [line.strip() for line in sys.stdin if line.strip()]
    triples, unreadable = provenance(paths)
    lines = ["| mutsu commit | rakudo | host | records |", "|---|---|---|---|"]
    for (commit, raku, host), count in sorted(triples.items()):
        lines.append(f"| `{commit}` | {raku} | `{host}` | {count} |")
    table = "\n".join(lines)
    print(table, file=sys.stderr)
    for problem in unreadable:
        print(f"::warning title=unreadable record::{problem}", file=sys.stderr)

    uniform = len(triples) == 1 and not unreadable
    if not uniform and triples:
        print("::warning title=mixed provenance::this sweep was not measured by a "
              "single (mutsu commit, rakudo, host) triple — the records still land, "
              "but no history.tsv row will be appended", file=sys.stderr)
    emit("uniform", "true" if uniform else "false")
    emit("records", sum(triples.values()))
    if args.markdown:
        with open(args.markdown, "w", encoding="utf-8") as fh:
            fh.write(table + "\n")
    return 0


# --- self-test ---------------------------------------------------------------

def self_test():
    failures = 0

    def check(label, got, expected):
        nonlocal failures
        if got != expected:
            print(f"not ok - {label}\n     got: {got}\nexpected: {expected}", file=sys.stderr)
            failures += 1
        else:
            print(f"ok - {label}")

    def fails(label, fn):
        nonlocal failures
        try:
            fn()
        except PlanError:
            print(f"ok - {label} (rejected)")
            return
        print(f"not ok - {label} (should have been rejected)", file=sys.stderr)
        failures += 1

    check("all fans out to 27 shards", len(plan("all")), 27)
    check("all shard args", plan("all")[0], {"id": "A", "args": "--prefix A"})
    check("all last shard is the non-alpha one", plan("all")[-1]["id"], "_")
    check("all single", plan("all", shards="single"), [{"id": "all", "args": "--all"}])
    check("stale is one job by default", plan("stale"),
          [{"id": "stale", "args": "--stale"}])
    check("stale fans out on request", plan("stale", shards="letters")[3],
          {"id": "D", "args": "--prefix D --stale"})
    check("status composes with the shard", plan("status", status="partial",
                                                 shards="letters")[0],
          {"id": "A", "args": "--prefix A --status partial"})
    check("status single", plan("status", status="green"),
          [{"id": "status-green", "args": "--status green"}])
    check("prefix is always one job", plan("prefix", prefix="s", shards="letters"),
          [{"id": "S", "args": "--prefix S"}])
    check("underscore prefix", plan("prefix", prefix="_"),
          [{"id": "_", "args": "--prefix _"}])
    check("only, comma separated", plan("only", only="BTree, Trie"),
          [{"id": "only", "args": "--only BTree --only Trie"}])
    check("only, whitespace separated", plan("only", only="A::B\nC-D"),
          [{"id": "only", "args": "--only A::B --only C-D"}])
    check("only never fans out", plan("only", only="BTree", shards="letters"),
          [{"id": "only", "args": "--only BTree"}])

    fails("shell metacharacter in only", lambda: plan("only", only="BTree; rm -rf /"))
    fails("flag smuggled through only", lambda: plan("only", only="--sandbox"))
    fails("empty only", lambda: plan("only", only="  "))
    fails("multi-letter prefix", lambda: plan("prefix", prefix="AB"))
    fails("digit prefix", lambda: plan("prefix", prefix="4"))
    fails("unknown status", lambda: plan("status", status="broken"))
    fails("unknown scope", lambda: plan("everything"))
    fails("unknown shards mode", lambda: plan("all", shards="many"))

    # Every generated token must be safe to word-split into an argv, since that
    # is exactly what the workflow does with it.
    tokens = [t for scope in ("all", "stale") for c in plan(scope, shards="letters")
              for t in c["args"].split()]
    check("no shell metacharacters in any generated arg",
          [t for t in tokens if not re.match(r"^[A-Za-z0-9:_.+-]+$", t.lstrip("-"))], [])

    triples, unreadable = provenance([])
    check("provenance of nothing is not uniform", (len(triples), unreadable), (0, []))

    print(f"\n{'FAILED' if failures else 'PASS'}: {failures} failure(s)")
    return 1 if failures else 0


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--self-test", action="store_true", help="verify plan/provenance and exit")
    sub = ap.add_subparsers(dest="cmd")

    p = sub.add_parser("plan", help="dispatch inputs -> job matrix JSON")
    p.add_argument("--scope", required=True,
                   choices=["all", "stale", "status", "prefix", "only"])
    p.add_argument("--prefix", default="")
    p.add_argument("--only", default="")
    p.add_argument("--status", default="")
    p.add_argument("--shards", default="auto", choices=["auto", "letters", "single"])
    p.set_defaults(func=cmd_plan)

    q = sub.add_parser("provenance", help="group records by (mutsu commit, rakudo, host)")
    q.add_argument("paths", nargs="*", help="record paths; read from stdin when absent")
    q.add_argument("--markdown", help="also write the table to this file")
    q.set_defaults(func=cmd_provenance)

    args = ap.parse_args()
    if args.self_test:
        return self_test()
    if not args.cmd:
        ap.error("pass a subcommand (plan / provenance) or --self-test")
    return args.func(args)


if __name__ == "__main__":
    raise SystemExit(main())
