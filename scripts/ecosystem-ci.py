#!/usr/bin/env python3
"""ecosystem-ci.py — the two decisions .github/workflows/ecosystem-sweep.yml has to make.

Both live here rather than inline in the workflow so they can be exercised
locally (`--self-test`) instead of being debugged one dispatched run at a time.

    scripts/ecosystem-ci.py plan --scope all --shards letters
    scripts/ecosystem-ci.py plan --scope only --only 'BTree, Trie'
    scripts/ecosystem-ci.py apply --source /tmp/incoming
    scripts/ecosystem-ci.py provenance ecosystem/dists/B/BTree.json …
    scripts/ecosystem-ci.py --self-test

`plan` turns the workflow's dispatch inputs into the job matrix: a list of
`{id, args}` chunks, each `args` a validated `ecosystem-sweep.py` selector. It
validates every input here, at plan time, so a typo fails in seconds instead of
after a build and a rakudo install — and so the workflow can word-split
`args` without smuggling anything into a shell.

`apply` copies a finished sweep's records onto the checkout, skipping any record
the base branch measured at a NEWER mutsu commit. That is what keeps a
multi-hour sweep from undoing an interpreter fix that landed while it ran.

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
import subprocess
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


# --- apply -------------------------------------------------------------------

def commit_is_ancestor(older, newer, repo="."):
    """True when `older` is an ancestor of `newer` (so `newer` is the later commit).

    Returns None when git cannot decide -- an unknown sha, a shallow clone.
    """
    if older == newer:
        return False
    proc = subprocess.run(["git", "-C", repo, "merge-base", "--is-ancestor", older, newer],
                          capture_output=True, text=True)
    if proc.returncode == 0:
        return True
    if proc.returncode == 1:
        return False
    return None


def record_measured(path):
    try:
        with open(path, encoding="utf-8") as fh:
            return json.load(fh)["measured"]
    except (OSError, ValueError, KeyError, TypeError):
        # TypeError covers a file that parses as JSON but is not an object --
        # any of these means "I cannot read a measurement here", and the caller
        # must then not claim one side supersedes the other.
        return None


def decide(incoming, existing, *, repo="."):
    """Which of two measurements of the same distribution should be kept?

    Returns ("apply"|"superseded"|"new", reason).

    The rule is "the newer mutsu commit wins", and it is not a tie-break
    convenience: a record says what mutsu did at one commit, so a record
    measured at a LATER commit is simply the more current answer. A sweep runs
    for over an hour, and an interpreter fix that lands during it re-measures
    the one distribution it fixed -- at a newer commit, by definition. Letting
    the sweep's older measurement overwrite that would silently undo the fix in
    the published ledger and re-open a record that is already green.
    """
    if existing is None:
        return "new", "no record on the base branch"
    inc, ext = record_measured(incoming), record_measured(existing)
    if inc is None or ext is None:
        return "apply", "unreadable record on one side"
    newer = commit_is_ancestor(inc["mutsu_commit"], ext["mutsu_commit"], repo=repo)
    if newer is True:
        return "superseded", f"base branch has {ext['mutsu_commit']}, newer than {inc['mutsu_commit']}"
    if newer is False:
        return "apply", f"ours ({inc['mutsu_commit']}) is not older than {ext['mutsu_commit']}"
    # Git could not order them: fall back on the recorded date, and when even
    # that ties, keep what is already on the branch. Never clobber a record we
    # cannot prove ours supersedes.
    if inc.get("date", "") > ext.get("date", ""):
        return "apply", "unorderable commits; ours is newer by date"
    return "superseded", "unorderable commits; keeping the base branch record"


def cmd_apply(args):
    """Copy a sweep's records over the checkout, skipping superseded ones."""
    src_root = os.path.join(args.source, "ecosystem")
    if not os.path.isdir(src_root):
        emit("applied", 0)
        emit("superseded", 0)
        print("nothing to apply", file=sys.stderr)
        return 0
    applied = superseded = 0
    notes = []
    for dirpath, _dirs, names in os.walk(src_root):
        for name in sorted(names):
            src = os.path.join(dirpath, name)
            rel = os.path.relpath(src, args.source)
            dest = os.path.join(args.repo, rel)
            # Only per-distribution records carry a measurement to compare;
            # everything else the sweep produced (index-snapshot.json) is ours.
            if re.match(r"^ecosystem/dists/.+\.json$", rel.replace(os.sep, "/")):
                verdict, why = decide(src, dest if os.path.exists(dest) else None, repo=args.repo)
            else:
                verdict, why = "apply", "not a distribution record"
            if verdict == "superseded":
                superseded += 1
                notes.append(f"{rel}: {why}")
                continue
            os.makedirs(os.path.dirname(dest), exist_ok=True)
            with open(src, "rb") as fh:
                data = fh.read()
            with open(dest, "wb") as fh:
                fh.write(data)
            applied += 1
    for note in notes:
        print(f"superseded {note}", file=sys.stderr)
    if superseded:
        print(f"::notice title=records superseded::{superseded} record(s) on the base branch "
              "were measured at a newer mutsu commit and were left alone", file=sys.stderr)
    emit("applied", applied)
    emit("superseded", superseded)
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

    # --- apply's decision rule, against a REAL throwaway git repo, because the
    # rule is "the newer mutsu commit wins" and only git can order two shas.
    import tempfile

    def git(repo, *args):
        subprocess.run(["git", "-C", repo, *args], check=True,
                       capture_output=True, text=True)

    with tempfile.TemporaryDirectory() as tmp:
        git(tmp, "init", "-q")
        git(tmp, "config", "user.email", "t@example.com")
        git(tmp, "config", "user.name", "t")
        shas = []
        for n in range(3):
            with open(os.path.join(tmp, "f"), "w", encoding="utf-8") as fh:
                fh.write(str(n))
            git(tmp, "add", "f")
            git(tmp, "commit", "-q", "-m", f"c{n}")
            shas.append(subprocess.run(["git", "-C", tmp, "rev-parse", "--short", "HEAD"],
                                       capture_output=True, text=True).stdout.strip())
        old_sha, mid_sha, new_sha = shas

        def rec(path, commit, date="2026-09-11"):
            full = os.path.join(tmp, path)
            os.makedirs(os.path.dirname(full), exist_ok=True)
            with open(full, "w", encoding="utf-8") as fh:
                json.dump({"measured": {"mutsu_commit": commit, "raku_version": "2026.07",
                                        "host": "h", "date": date}}, fh)
            return full

        ours = rec("ours.json", old_sha)
        theirs_newer = rec("newer.json", new_sha)
        theirs_older = rec("older.json", old_sha)
        theirs_same = rec("same.json", old_sha)
        mid = rec("mid.json", mid_sha)

        check("ancestry: old is an ancestor of new",
              commit_is_ancestor(old_sha, new_sha, repo=tmp), True)
        check("ancestry: new is not an ancestor of old",
              commit_is_ancestor(new_sha, old_sha, repo=tmp), False)
        check("ancestry: a commit is not treated as newer than itself",
              commit_is_ancestor(old_sha, old_sha, repo=tmp), False)
        check("ancestry: an unknown sha is undecidable",
              commit_is_ancestor("0" * 12, new_sha, repo=tmp), None)

        check("no record on the base branch -> new",
              decide(ours, None, repo=tmp)[0], "new")
        check("base branch measured later -> superseded",
              decide(ours, theirs_newer, repo=tmp)[0], "superseded")
        check("base branch measured earlier -> apply",
              decide(rec("o2.json", new_sha), theirs_older, repo=tmp)[0], "apply")
        check("same commit -> apply (ours is the fresher measurement)",
              decide(ours, theirs_same, repo=tmp)[0], "apply")
        check("mid commit is still newer than ours -> superseded",
              decide(ours, mid, repo=tmp)[0], "superseded")

        unknown = rec("unknown.json", "0" * 12)
        check("unorderable, ours newer by date -> apply",
              decide(rec("o3.json", old_sha, date="2026-09-12"), unknown, repo=tmp)[0], "apply")
        check("unorderable, no date edge -> keep the base branch",
              decide(rec("o4.json", old_sha, date="2026-09-01"), unknown, repo=tmp)[0],
              "superseded")
        check("an unreadable side is never silently dropped",
              decide(ours, os.path.join(tmp, "f"), repo=tmp)[0], "apply")

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

    a = sub.add_parser("apply", help="copy a sweep's records over the checkout")
    a.add_argument("--source", required=True,
                   help="directory holding the downloaded artifacts (an ecosystem/ tree)")
    a.add_argument("--repo", default=".", help="the checkout to apply them to")
    a.set_defaults(func=cmd_apply)

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
