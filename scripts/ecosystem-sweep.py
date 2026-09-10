#!/usr/bin/env python3
"""ecosystem-sweep.py — measure mutsu against rakudo, one distribution at a time.

Runs each zef distribution's own test suite under BOTH interpreters, with the
same sources, the same `-I` list, the same working directory and the same
timeout, and records both sides per test file. rakudo is the denominator: a file
rakudo does not pass cleanly is excluded from the KPI, never charged to mutsu.

Decisions: docs/adr/0085-ecosystem-testsuite-parity-measurement.md
Operations: docs/ecosystem-parity.md   Tracking issue: #7785

    scripts/ecosystem-sweep.py --only String::Utils
    scripts/ecosystem-sweep.py --prefix A --jobs 8
    scripts/ecosystem-sweep.py --all --jobs 8
    scripts/ecosystem-sweep.py --rollup

Env: MUTSU_BIN (default target/release/mutsu), RAKU_BIN (default raku).
"""

from __future__ import annotations

import argparse
import collections
import concurrent.futures
import datetime as dt
import json
import os
import re
import subprocess
import sys
import tempfile
import threading

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ecosystem_common as eco  # noqa: E402

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DATA_DIR = os.path.join(REPO, "ecosystem")
DISTS_DIR = os.path.join(DATA_DIR, "dists")
SCHEMA = 1
HARNESS = 1

# `use Test` must reach the vendored upstream Test.rakumod on both sides, or the
# two are counting `ok` lines emitted by different harnesses. The module exports
# its own MONKEY-SEE-NO-EVAL, which no reimplementation ever had -- the same
# decisive probe t/tooling/vendored-real-test-module.t uses.
TEST_PROBE = 'use Test; plan 1; is MONKEY-SEE-NO-EVAL(), 1, "vendored Test"'

# Two locks, deliberately: `log()` takes the print lock, so anything that holds
# a lock across a `log()` call must hold a DIFFERENT one. A single shared
# `threading.Lock` here deadlocked every `--jobs > 1` run the moment the first
# worker finished.
_print_lock = threading.Lock()
_tally_lock = threading.Lock()


def log(msg):
    with _print_lock:
        print(msg, file=sys.stderr, flush=True)


# --- running -----------------------------------------------------------------

def run(cmd, cwd, timeout, sandbox, sbx_home, writable=()):
    """One interpreter run. Returns (returncode, combined output)."""
    if sandbox:
        cmd = eco.sandbox_wrap(cmd, cwd, sbx_home, writable=writable)
    # MUTSU_FUDGE is roast-only: with it set, a stray `#?rakudo skip` comment in
    # a distribution would silently drop the next statement.
    env = {k: v for k, v in os.environ.items() if k != "MUTSU_FUDGE"}
    try:
        proc = subprocess.run(cmd, capture_output=True, text=True, errors="replace",
                              timeout=timeout, cwd=cwd, env=env)
        return proc.returncode, proc.stdout + proc.stderr
    except subprocess.TimeoutExpired:
        return None, "SWEEP-TIMEOUT"


def side_result(rc, out, secs):
    plan, ok, notok, todo, skip = eco.parse_tap(out)
    verdict = "timeout" if out == "SWEEP-TIMEOUT" else eco.tap_verdict(out, rc)
    side = {"verdict": verdict, "plan": plan, "ok": ok, "nok": notok,
            "todo": todo, "skip": skip, "secs": round(secs, 2)}
    if verdict != "pass":
        detail = (eco.first_failing_assertion(out) if verdict == "fail"
                  else eco.first_error_line(out))
        if detail:
            side["first_failure"] = detail
    return side


def measure(cmd, cwd, timeout, sandbox, sbx_home, writable, attempts):
    """Run a file, retrying only when it did not pass.

    A suite that flakes must not move the KPI, and retrying everything would
    triple the cost of a green corpus -- so the retry is spent only where the
    answer was not already 'pass'. Returns (side, flaky).
    """
    best = None
    verdicts = set()
    for _ in range(attempts):
        start = dt.datetime.now()
        rc, out = run(cmd, cwd, timeout, sandbox, sbx_home, writable)
        side = side_result(rc, out, (dt.datetime.now() - start).total_seconds())
        verdicts.add(side["verdict"])
        if best is None or side["verdict"] == "pass":
            best = side
        if side["verdict"] == "pass":
            break
    return best, len(verdicts) > 1


def compare(raku, mutsu):
    if raku["verdict"] != "pass":
        return "no_baseline"
    if mutsu["verdict"] == "pass":
        return "parity"
    if mutsu["verdict"] == "fail":
        return "partial"
    return "regression"


# --- one distribution --------------------------------------------------------

def sweep_dist(name, index, opts, bundled) -> dict:
    """Measure one distribution and return its record."""
    record = {
        "schema": SCHEMA, "dist": name,
        "version": (index.dists[name].get("version") or "?"),
        "source": {"index": "fez", "url": index.url(name)},
        "measured": {
            "date": dt.date.today().isoformat(),
            "mutsu_commit": opts.mutsu_commit, "mutsu_version": opts.mutsu_version,
            "raku_version": opts.raku_version, "raku_backend": opts.raku_backend,
            "host": opts.host, "sandbox": "bwrap" if opts.sandbox else "none",
            # How many chances each side got. A record measured at 1 has no
            # flake protection, so it is weaker evidence than one at 3 -- say so
            # in the record rather than leaving the reader to guess.
            "attempts": opts.attempts,
            "harness": HARNESS,
        },
        "files": [],
    }

    resolved, unresolved = index.closure(name)
    record["deps"] = {"mode": "flat-closure", "resolved": resolved,
                      "unresolved": unresolved, "bundled_would_supply": []}
    if unresolved:
        # ADR-0085 D5: run on neither side rather than let mutsu's bundled
        # batteries supply what rakudo lacks -- that would raise the KPI with no
        # compatibility improving. What the bundle *would* have supplied is
        # recorded here as a batteries statistic instead.
        record["deps"]["bundled_would_supply"] = sorted(set(unresolved) & bundled)
        record["status"] = "blocked_dep"
        record["totals"] = empty_totals()
        return record

    workdir = tempfile.mkdtemp(prefix="ecosweep-", dir=opts.tmp_root)
    try:
        root = prepare(name, index, workdir, opts, resolved)
        if root is None:
            record["status"] = "skipped"
            record["note"] = "no META6.json in the tarball"
            record["totals"] = empty_totals()
            return record
        meta = eco.read_meta6(root) or {}
        record["version"] = meta.get("version", record["version"])
        record["axis"] = eco.source_axis(root)

        libs = ["-I", os.path.join(root, "lib")]
        for dep in resolved:
            dep_root = opts.dep_roots.get(dep)
            if dep_root:
                libs += ["-I", os.path.join(dep_root, "lib")]

        sbx_home = os.path.join(workdir, "sbxhome")
        os.makedirs(sbx_home, exist_ok=True)
        common = dict(cwd=root, timeout=opts.timeout, sandbox=opts.sandbox,
                      sbx_home=sbx_home, writable=(root,))

        # Load probe, both sides. A module rakudo cannot load either is not a
        # mutsu finding -- the standing rule is to check raku before calling
        # anything a mutsu bug.
        record["load"] = {}
        blocked = False
        for module in sorted(meta.get("provides") or {}):
            probe = ["-e", f"use {module}; exit 0"]
            m_rc, m_out = run([opts.mutsu] + libs + probe, **common)
            if m_rc == 0:
                record["load"][module] = "ok"
                continue
            r_rc, _r_out = run([opts.raku] + libs + probe, **common)
            if r_rc != 0:
                record["load"][module] = "raku_also_fails"
            else:
                record["load"][module] = eco.first_error_line(m_out) or "load failed"
                blocked = True
        if blocked:
            record["status"] = "blocked_load"
            record["totals"] = empty_totals()
            return record

        tests = eco.find_test_files(root, include_xt=opts.include_xt)
        if opts.max_files:
            tests = tests[:opts.max_files]
        for path in tests:
            rel = os.path.relpath(path, root)
            # Both sides get the same retry budget. Giving mutsu three chances
            # at a file the baseline got one is not a fair comparison, even
            # though the asymmetry would only ever shrink the denominator.
            raku, raku_flaky = measure([opts.raku] + libs + [path],
                                       attempts=opts.attempts, **common)
            if raku["verdict"] != "pass":
                record["files"].append({"path": rel, "raku": raku, "cmp": "no_baseline"})
                continue
            mutsu, flaky = measure([opts.mutsu] + libs + [path],
                                   attempts=opts.attempts, **common)
            entry = {"path": rel, "raku": raku, "mutsu": mutsu,
                     "cmp": compare(raku, mutsu)}
            if flaky or raku_flaky:
                entry["flaky"] = True
                entry["cmp"] = "no_baseline"
            record["files"].append(entry)
    finally:
        eco.rmtree(workdir)

    record["totals"] = totals_of(record["files"])
    record["status"] = status_of(record["totals"])
    return record


def prepare(name, index, workdir, opts, resolved):
    """Extract the distribution under test into a throwaway directory, and every
    dependency into the shared extract cache (they are read-only and reused)."""
    url = index.url(name)
    if not url:
        return None
    root = eco.extract_dist(eco.fetch_tarball(url, opts.tarball_cache),
                            os.path.join(workdir, "dist"))
    for dep in resolved:
        if dep in opts.dep_roots:
            continue
        dep_url = index.url(dep)
        if not dep_url:
            continue
        target = os.path.join(opts.extract_cache, re.sub(r"[^A-Za-z0-9._-]", "_", dep))
        with opts.dep_lock:
            if dep not in opts.dep_roots:
                marker = os.path.join(target, ".ok")
                if os.path.isdir(target) and os.path.exists(marker):
                    with open(marker, encoding="utf-8") as fh:
                        opts.dep_roots[dep] = fh.read().strip()
                else:
                    eco.rmtree(target)
                    dep_root = eco.extract_dist(
                        eco.fetch_tarball(dep_url, opts.tarball_cache), target)
                    if dep_root:
                        with open(marker, "w", encoding="utf-8") as fh:
                            fh.write(dep_root)
                        opts.dep_roots[dep] = dep_root
    return root


def empty_totals():
    return {"baseline_files": 0, "parity_files": 0, "regressed_files": 0,
            "baseline_assertions": 0, "mutsu_assertions": 0}


def totals_of(files):
    t = empty_totals()
    for f in files:
        if f["cmp"] in ("no_baseline",):
            continue
        t["baseline_files"] += 1
        t["baseline_assertions"] += f["raku"]["ok"]
        t["mutsu_assertions"] += min(f["mutsu"]["ok"], f["raku"]["ok"])
        if f["cmp"] == "parity":
            t["parity_files"] += 1
        else:
            t["regressed_files"] += 1
    return t


def status_of(t):
    if t["baseline_files"] == 0:
        return "no_baseline"
    if t["parity_files"] == t["baseline_files"]:
        return "green"
    if t["parity_files"] == 0:
        return "red"
    return "partial"


# --- the record store --------------------------------------------------------

def shard_of(name):
    first = name[0].upper()
    return first if first.isalpha() and first.isascii() else "_"


def record_path(name):
    return os.path.join(DISTS_DIR, shard_of(name), name.replace("::", "--") + ".json")


def write_record(record):
    path = record_path(record["dist"])
    os.makedirs(os.path.dirname(path), exist_ok=True)
    text = json.dumps(record, indent=2, sort_keys=True, ensure_ascii=False) + "\n"
    # Date-granular provenance means an unchanged same-day re-run is a no-op in
    # git rather than churn (ADR-0085 D7).
    if os.path.exists(path):
        with open(path, encoding="utf-8") as fh:
            if fh.read() == text:
                return False
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(text)
    return True


def load_records():
    out = []
    for dirpath, _dirs, names in os.walk(DISTS_DIR):
        for n in sorted(names):
            if n.endswith(".json"):
                with open(os.path.join(dirpath, n), encoding="utf-8") as fh:
                    out.append(json.load(fh))
    return out


# --- rollup ------------------------------------------------------------------

def rollup(append_history=False):
    records = load_records()
    if not records:
        sys.exit(f"no records under {DISTS_DIR} — run a sweep first")
    by_status = collections.Counter(r["status"] for r in records)
    baseline = sum(r["totals"]["baseline_files"] for r in records)
    parity = sum(r["totals"]["parity_files"] for r in records)
    b_assert = sum(r["totals"]["baseline_assertions"] for r in records)
    m_assert = sum(r["totals"]["mutsu_assertions"] for r in records)
    graded = [r for r in records if r["totals"]["baseline_files"] > 0]
    green = [r for r in graded if r["status"] == "green"]

    def pct(num, den):
        return round(100.0 * num / den, 1) if den else 0.0

    summary = {
        "schema": SCHEMA,
        "generated": dt.date.today().isoformat(),
        "distributions": len(records),
        "status": dict(sorted(by_status.items())),
        "dist_parity": pct(len(green), len(graded)),
        "file_parity": pct(parity, baseline),
        "assertion_parity": pct(m_assert, b_assert),
        "baseline_files": baseline, "parity_files": parity,
        "baseline_assertions": b_assert, "mutsu_assertions": m_assert,
        "graded_distributions": len(graded), "green_distributions": len(green),
    }
    os.makedirs(DATA_DIR, exist_ok=True)
    with open(os.path.join(DATA_DIR, "summary.json"), "w", encoding="utf-8") as fh:
        json.dump(summary, fh, indent=2, sort_keys=True)
        fh.write("\n")
    write_summary_md(summary, records)
    if append_history:
        append_history_row(summary, records)
    render_chart()
    return summary


def write_summary_md(summary, records):
    lines = [
        "# Ecosystem parity — summary",
        "",
        "<!-- GENERATED by scripts/ecosystem-sweep.py --rollup. Do not edit. -->",
        "",
        f"Generated {summary['generated']} from `ecosystem/dists/`. Method and metric",
        "definitions: [docs/ecosystem-parity.md](../docs/ecosystem-parity.md).",
        "",
        "| metric | value |",
        "|---|---|",
        f"| **dist parity** (published headline) | **{summary['dist_parity']}%** "
        f"({summary['green_distributions']}/{summary['graded_distributions']}) |",
        f"| file parity | {summary['file_parity']}% "
        f"({summary['parity_files']}/{summary['baseline_files']}) |",
        f"| assertion parity | {summary['assertion_parity']}% |",
        "",
        "| status | distributions |",
        "|---|---|",
    ]
    for status, count in summary["status"].items():
        lines.append(f"| `{status}` | {count} |")
    lines += ["", "## Distributions", "",
              "| distribution | version | status | baseline files passed |", "|---|---|---|---|"]
    for r in sorted(records, key=lambda r: r["dist"].lower()):
        t = r["totals"]
        passed = f"{t['parity_files']}/{t['baseline_files']}" if t["baseline_files"] else "—"
        lines.append(f"| `{r['dist']}` | {r['version']} | {r['status']} | {passed} |")
    with open(os.path.join(DATA_DIR, "summary.md"), "w", encoding="utf-8") as fh:
        fh.write("\n".join(lines) + "\n")


def append_history_row(summary, records):
    path = os.path.join(DATA_DIR, "history.tsv")
    columns = ["date", "mutsu_commit", "raku_version", "dists", "measured",
               "blocked_dep", "baseline_files", "parity_files", "file_parity",
               "assertion_parity", "dist_parity"]
    latest = max(records, key=lambda r: r["measured"]["date"])["measured"]
    row = {
        "date": summary["generated"],
        "mutsu_commit": latest["mutsu_commit"], "raku_version": latest["raku_version"],
        "dists": summary["distributions"],
        "measured": summary["distributions"] - summary["status"].get("blocked_dep", 0),
        "blocked_dep": summary["status"].get("blocked_dep", 0),
        "baseline_files": summary["baseline_files"], "parity_files": summary["parity_files"],
        "file_parity": summary["file_parity"], "assertion_parity": summary["assertion_parity"],
        "dist_parity": summary["dist_parity"],
    }
    exists = os.path.exists(path)
    with open(path, "a", encoding="utf-8") as fh:
        if not exists:
            fh.write("\t".join(columns) + "\n")
        fh.write("\t".join(str(row[c]) for c in columns) + "\n")


def render_chart():
    history = os.path.join(DATA_DIR, "history.tsv")
    if not os.path.exists(history):
        return
    subprocess.run([sys.executable, os.path.join(REPO, "scripts", "plot-ecosystem-history.py"),
                    history, os.path.join(DATA_DIR, "history.svg")], check=False)


# --- preflight ---------------------------------------------------------------

def preflight(args):
    mutsu = os.path.abspath(os.environ.get("MUTSU_BIN", "target/release/mutsu"))
    raku = os.environ.get("RAKU_BIN", "raku")
    if not os.path.exists(mutsu):
        sys.exit(f"mutsu binary not found: {mutsu} (build it or set MUTSU_BIN)")
    if os.environ.get("MUTSU_FUDGE"):
        sys.exit("MUTSU_FUDGE is set; it is roast-only and would drop statements. Unset it.")

    def probe(binary):
        p = subprocess.run([binary, "-e", TEST_PROBE], capture_output=True, text=True,
                           timeout=120, cwd=REPO)
        return "ok 1 - vendored Test" in p.stdout

    if not probe(mutsu):
        sys.exit("mutsu did not answer `use Test` with the vendored Test.rakumod "
                 "(ADR-0085 D8) — refusing to publish a flattered number")
    if not probe(raku):
        sys.exit(f"the rakudo oracle at {raku!r} is not usable — install it "
                 "(.agents/skills/install-raku/install-raku.sh)")

    version = subprocess.run([raku, "-e", "print $*RAKU.compiler.version"],
                             capture_output=True, text=True).stdout.strip()
    backend = subprocess.run([raku, "-e", "print $*VM.name ~ ' ' ~ $*VM.version"],
                             capture_output=True, text=True).stdout.strip()
    commit = subprocess.run(["git", "-C", REPO, "rev-parse", "--short", "HEAD"],
                            capture_output=True, text=True).stdout.strip()
    mutsu_version = subprocess.run([mutsu, "--version"], capture_output=True,
                                   text=True).stdout.strip().split()[-1]

    sandbox = args.sandbox != "none"
    if sandbox and not eco.have_bwrap():
        sys.exit("bwrap not found. A corpus sweep runs unaudited test suites and is not "
                 "supported unsandboxed: apt-get install bubblewrap (or --sandbox none "
                 "for a single distribution you already trust).")
    if not sandbox and not (args.only or args.dry_run):
        sys.exit("--sandbox none is for a single --only distribution, never a corpus sweep")

    args.mutsu, args.raku = mutsu, raku
    args.raku_version, args.raku_backend = version, backend
    args.mutsu_commit, args.mutsu_version = commit, mutsu_version
    args.host = f"{os.uname().sysname.lower()}-{os.uname().machine}"
    args.sandbox = sandbox
    args.tarball_cache = os.path.join(eco.CACHE_DIR, "tarballs")
    args.extract_cache = os.path.join(eco.CACHE_DIR, "deps")
    args.tmp_root = os.path.join(eco.CACHE_DIR, "run")
    os.makedirs(args.extract_cache, exist_ok=True)
    os.makedirs(args.tmp_root, exist_ok=True)
    args.dep_roots = {}
    args.dep_lock = threading.Lock()
    return args


def select(index, args):
    if args.only:
        missing = [n for n in args.only if n not in index.targets]
        for n in missing:
            log(f"warning: {n} is not in the index")
        return [n for n in args.only if n in index.targets]
    names = sorted(index.targets)
    if args.prefix:
        names = [n for n in names if shard_of(n) == args.prefix.upper()]
    if args.status:
        keep = []
        for n in names:
            path = record_path(n)
            if os.path.exists(path):
                with open(path, encoding="utf-8") as fh:
                    if json.load(fh)["status"] == args.status:
                        keep.append(n)
        names = keep
    if args.stale:
        keep = []
        for n in names:
            path = record_path(n)
            if not os.path.exists(path):
                keep.append(n)
                continue
            with open(path, encoding="utf-8") as fh:
                m = json.load(fh)["measured"]
            if m["mutsu_commit"] != args.mutsu_commit or m["raku_version"] != args.raku_version:
                keep.append(n)
        names = keep
    return names


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--only", action="append", default=[], help="measure these distributions")
    ap.add_argument("--prefix", help="measure one shard, e.g. A")
    ap.add_argument("--all", action="store_true", help="measure the whole corpus")
    ap.add_argument("--status", help="only distributions whose current record has this status")
    ap.add_argument("--stale", action="store_true",
                    help="only records measured at another mutsu commit or rakudo version")
    ap.add_argument("--rollup", action="store_true",
                    help="regenerate summary.json / summary.md / history.svg and exit")
    ap.add_argument("--history", action="store_true",
                    help="with --rollup, also append a history.tsv row (full sweeps only)")
    ap.add_argument("--jobs", type=int, default=1)
    ap.add_argument("--timeout", type=int, default=120, help="per test file, both sides")
    ap.add_argument("--attempts", type=int, default=3, help="retries for a non-passing file")
    ap.add_argument("--max-files", type=int, default=0, help="cap files per distribution")
    ap.add_argument("--include-xt", action="store_true")
    ap.add_argument("--sandbox", choices=["bwrap", "none"], default="bwrap")
    ap.add_argument("--refresh-index", action="store_true")
    ap.add_argument("--dry-run", action="store_true", help="resolve and print the plan only")
    args = ap.parse_args()

    if args.rollup:
        summary = rollup(append_history=args.history)
        print(json.dumps(summary, indent=2, sort_keys=True))
        return 0
    if not (args.only or args.prefix or args.all or args.status or args.stale):
        ap.error("nothing selected: pass --only / --prefix / --all / --status / --stale")

    args = preflight(args)
    index = eco.load_index(refresh=args.refresh_index)
    os.makedirs(DATA_DIR, exist_ok=True)
    with open(os.path.join(DATA_DIR, "index-snapshot.json"), "w", encoding="utf-8") as fh:
        json.dump({"fetched": dt.date.today().isoformat(), **index.snapshot}, fh,
                  indent=2, sort_keys=True)
        fh.write("\n")

    bundled = bundled_modules()
    names = select(index, args)
    log(f"sandbox: {'bwrap' if args.sandbox else 'NONE'} | mutsu {args.mutsu_commit} | "
        f"rakudo {args.raku_version} | {len(names)} distribution(s)")
    if args.dry_run:
        for n in names:
            resolved, unresolved = index.closure(n)
            log(f"{n:40} deps={len(resolved)} unresolved={unresolved or '-'}")
        return 0

    done = collections.Counter()

    def work(name):
        record = sweep_dist(name, index, args, bundled)
        changed = write_record(record)
        with _tally_lock:
            done[record["status"]] += 1
            seen = sum(done.values())
        t = record["totals"]
        log(f"[{seen}/{len(names)}] {name:40} {record['status']:12} "
            f"{t['parity_files']}/{t['baseline_files']} files"
            f"{'' if changed else '  (unchanged)'}")

    if args.jobs > 1:
        with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as pool:
            list(pool.map(work, names))
    else:
        for name in names:
            work(name)

    log("\n=== sweep complete ===")
    for status, count in sorted(done.items()):
        log(f"  {status:14} {count}")
    log("\nrun `--rollup` to regenerate summary.json / summary.md"
        + (" / history.tsv" if args.all else ""))
    return 0


def bundled_modules() -> set[str]:
    """Distribution and module names mutsu bundles under modules/."""
    names = set()
    modules_dir = os.path.join(REPO, "modules")
    for entry in sorted(os.listdir(modules_dir)) if os.path.isdir(modules_dir) else []:
        meta = eco.read_meta6(os.path.join(modules_dir, entry))
        if not meta:
            continue
        if meta.get("name"):
            names.add(meta["name"])
        names.update(meta.get("provides") or {})
    return names


if __name__ == "__main__":
    raise SystemExit(main())
