#!/usr/bin/env python3
"""
dist-compat-sweep.py — actually RUN real fez-ecosystem distributions under mutsu
and record, per dist, whether they load and why they fail.

This is the execution counterpart to scripts/ecosystem-guts-survey.py. That tool
only *signal-scans* sources ("pure_raku" = "not guts-blocked", explicitly NOT
"runs on mutsu"). This tool downloads each dist, points mutsu at its own lib, and
tries to `use` every module the dist `provides`, bucketing the outcome by root
cause. The result feeds the live dashboard docs/dist-compat-sweep.md.

Method, per sampled dist:
  1. Read the local fez index (~/.zef/store/fez/fez.json — the same one mzef uses),
     keep the latest version of each dist, draw a fixed-seed random sample.
  2. Download the tarball from the fez CDN (cached under CACHE_DIR), extract it.
  3. Read META6.json `provides` (Module::Name -> source path). Skip dists whose
     sources carry a compiler-guts / NativeCall signal (a separate axis; see the
     guts survey) unless --include-guts / --include-native.
  4. For each provided module run:  mutsu -I <dist>/lib [-I <extra>...] -e 'use M'
     with a timeout, and classify the FIRST failure:

  Bucket        Meaning                                    Reachable by
  ------------  -----------------------------------------  ----------------------
  load_ok       every provided module loads                (already works)
  missing_dep   `Could not find <X>` where X != this dist  install X first / dep sweep
  parse_error   ===SORRY!=== / Confused / parse failure    parser fix
  panic         Rust panic (thread '...' panicked)         VM/interpreter bug (top prio)
  timeout       exceeded --timeout                          hang / perf bug
  runtime_error any other non-zero exit                    a general mutsu bug

`missing_dep` is real signal, not a mutsu bug: it says "reachable once its deps
are present". Re-run with --site-repo pointing at a HOME whose site repo has the
deps installed (e.g. after `mzef install`) to collapse those into load_ok/other.

Usage:
  scripts/dist-compat-sweep.py [--n N] [--seed S] [--timeout SEC]
                               [--include-guts] [--include-native]
                               [--site-repo DIR] [--extra-lib DIR]...
                               [--tsv PATH] [--only NAME]...
Output: a per-dist TSV (default tmp/dist-compat-sweep.tsv) + a summary + a
markdown table block to paste into docs/dist-compat-sweep.md.

Env: MUTSU_BIN (default target/release/mutsu).
"""
import argparse, collections, io, json, os, re, shutil, subprocess, sys, tarfile
import tempfile, urllib.request

FEZ_INDEX = os.path.expanduser("~/.zef/store/fez/fez.json")
FEZ_CDN = "https://360.zef.pm/"
CACHE_DIR = os.path.expanduser("~/.cache/mutsu-dist-sweep")

DEEP = re.compile(
    r"QAST|Metamodel::Primitives|NQPHLL|:from<NQP>|EXPORTHOW|define_slang|"
    r"package_declarator:sym|use\s+experimental\s*:\s*macro|Slang::"
)
NATIVE = re.compile(r"\buse\s+NativeCall\b|:from<C>|\bis\s+native\b")
SRC_RE = re.compile(r"\.(rakumod|pm6|pm|raku|rakutest|t)$")
TEST_RE = re.compile(r"\.(t|rakutest)$")

PLAN_RE = re.compile(r"^1\.\.(\d+)\s*$", re.M)
OK_RE = re.compile(r"^ok\b", re.M)
NOTOK_RE = re.compile(r"^not ok\b")


def find_test_files(root):
    """Test files under the dist's t/ (and test/, xt/) directories."""
    files = []
    for sub in ("t", "test", "xt"):
        d = os.path.join(root, sub)
        if not os.path.isdir(d):
            continue
        for dirpath, _, names in os.walk(d):
            for nm in names:
                if TEST_RE.search(nm):
                    files.append(os.path.join(dirpath, nm))
    return sorted(files)


def parse_tap(out):
    """(planned, ok_count, real_not_ok, todo_not_ok) from raw TAP output."""
    m = PLAN_RE.search(out)
    plan = int(m.group(1)) if m else None
    ok = len(OK_RE.findall(out))
    real_notok = todo = 0
    for line in out.splitlines():
        if NOTOK_RE.match(line):
            if "todo" in line.lower():
                todo += 1
            else:
                real_notok += 1
    return plan, ok, real_notok, todo


def tap_verdict(out, rc):
    """Classify a single test run: 'pass' / 'fail' / 'die'.
    'die' = crashed before/mid TAP (no plan, or ran fewer than planned, or a
    non-zero exit with no failing assertion to blame). 'fail' = a real `not ok`.
    """
    plan, ok, notok, todo = parse_tap(out)
    if plan is None:
        return "die"
    if notok > 0:
        return "fail"
    ran = ok + notok + todo
    if ran < plan:
        return "die"
    if rc not in (0, None):
        return "die"
    return "pass"


# Generic TAP-harness death lines that mask the real cause — never a useful
# signature on their own.
_HARNESS_NOISE = re.compile(
    r"test failures|you planned|you failed|looks like you|^#|^1\.\.|dubious|"
    r"^ok\b|^not ok\b", re.I)


def first_error_line(out):
    """The first meaningful error line, skipping generic TAP-harness noise
    ('Runtime error: Test failures', '# You planned N ...') that only reports
    that the run died, not why."""
    candidates = []
    for line in out.splitlines():
        s = line.strip()
        if not s or _HARNESS_NOISE.search(s):
            continue
        candidates.append(s)
    for s in candidates:
        low = s.lower()
        if ("sorry" in low or "panicked" in low or "unhandled" in low
                or s.startswith("X::") or "::" in s and "exception" in low
                or "no such" in low or "unknown method" in low
                or "unknown function" in low or "cannot" in low
                or low.startswith("runtime error")):
            return s[:200]
    return candidates[0][:200] if candidates else ""


def first_failing_assertion(out):
    """The description of the first real (non-TODO) `not ok` line, e.g.
    'not ok 3 - foo does bar' -> 'foo does bar'. Falls back to first_error_line."""
    for line in out.splitlines():
        if NOTOK_RE.match(line) and "todo" not in line.lower():
            m = re.match(r"not ok\s+\d+\s*-?\s*(.*)", line.strip())
            desc = (m.group(1).strip() if m else line.strip())
            return desc[:200] if desc else line.strip()[:200]
    return first_error_line(out)


def version_key(v):
    return [(0, int(x)) if x.isdigit() else (1, x) for x in re.split(r"[.\-+]", str(v))]


def sandbox_wrap(cmd, root, sbx_home, mem_kb=6_000_000, nproc=400):
    """Wrap `cmd` so untrusted dist code runs with NO network, a read-only
    filesystem, an isolated throwaway HOME, its own PID namespace, and rlimits.
    `sbx_home` must be an existing dir (a fresh tmpfs is mounted over it, so
    nothing the code writes ever reaches the host). Requires bubblewrap (bwrap).

    NOTE: `use <module>` executes arbitrary BEGIN/CHECK phasers and load-time
    code from a real ecosystem dist — this is arbitrary code execution, hence
    the sandbox. Download/extract happen OUTSIDE the sandbox (in Python); only
    the mutsu run is confined, and it needs no network.
    """
    return [
        "bwrap",
        "--unshare-all",              # user+net+pid+ipc+uts+cgroup+mount (net = offline)
        "--ro-bind", "/", "/",        # whole rootfs, read-only
        "--dev", "/dev",
        "--proc", "/proc",
        "--tmpfs", "/run",
        "--tmpfs", sbx_home,          # writable throwaway HOME (tmpfs over existing dir)
        "--setenv", "HOME", sbx_home,
        "--chdir", root,
        "--die-with-parent",
        "--new-session",
        "bash", "-c", f"ulimit -v {mem_kb} -u {nproc} 2>/dev/null; exec \"$@\"", "_",
    ] + cmd


def have_bwrap():
    return shutil.which("bwrap") is not None


def load_index():
    if not os.path.exists(FEZ_INDEX):
        sys.exit(f"missing {FEZ_INDEX} — run an mzef/zef ecosystem op first")
    best = {}
    for m in json.load(open(FEZ_INDEX)):
        if isinstance(m, dict) and m.get("name") and m.get("path"):
            n = m["name"]
            if n not in best or version_key(m.get("version", "0")) > version_key(
                best[n].get("version", "0")
            ):
                best[n] = m
    return best


def fetch_tarball(meta):
    os.makedirs(CACHE_DIR, exist_ok=True)
    cache = os.path.join(CACHE_DIR, meta["path"].replace("/", "_"))
    if not os.path.exists(cache):
        data = urllib.request.urlopen(FEZ_CDN + meta["path"], timeout=60).read()
        with open(cache, "wb") as f:
            f.write(data)
    return cache


def classify(module, dist_name, rc, out):
    if rc == -9 or "SWEEP-TIMEOUT" in out:
        return "timeout", "timed out"
    if "panicked" in out or "RUST_BACKTRACE" in out:
        first = next((l for l in out.splitlines() if "panicked" in l), out[:200])
        return "panic", first.strip()
    m = re.search(r"Could not find ([\w:]+) in", out)
    if m and m.group(1) != dist_name and m.group(1) != module:
        return "missing_dep", m.group(1)
    if m:  # could not find the dist's own module — packaging/name mismatch
        return "runtime_error", f"self-module not found: {m.group(1)}"
    if "===SORRY!===" in out or "Confused" in out or "Unable to parse" in out:
        first = next(
            (l for l in out.splitlines() if l.strip() and "SORRY" not in l), ""
        )
        return "parse_error", first.strip()[:200]
    if rc == 0:
        return "load_ok", ""
    first = next((l for l in out.splitlines() if l.strip()), "")
    return "runtime_error", first.strip()[:200]


def test_dist(name, version, axis, root, mutsu, libs, timeout, sandbox, sbx_home,
              max_files=25):
    """Run the dist's own test suite, using raku as the baseline. Only files that
    raku itself passes cleanly count (others need missing deps or are the dist's
    own bug). For those, classify mutsu as pass/fail/die and roll up to one row:
      test_pass  — mutsu passes every raku-clean file
      test_fail  — at least one raku-clean file has a real `not ok` in mutsu
      test_die   — at least one raku-clean file crashes in mutsu (die > fail)
      test_no_baseline — no test file that raku passes cleanly (nothing to compare)
    """
    tests = find_test_files(root)[:max_files]
    if not tests:
        return (name, version, "test_notest", "no test files", axis)
    n_base = n_pass = n_fail = n_die = 0
    first_die = first_fail = ""
    for tf in tests:
        rel = os.path.relpath(tf, root)
        rk = ["raku"] + libs + [tf]
        rk = sandbox_wrap(rk, root, sbx_home) if sandbox else rk
        try:
            rp = subprocess.run(rk, capture_output=True, text=True,
                                timeout=timeout, cwd=root)
        except subprocess.TimeoutExpired:
            continue
        if tap_verdict(rp.stdout + rp.stderr, rp.returncode) != "pass":
            continue  # not a clean raku baseline — skip
        n_base += 1
        mt = [mutsu] + libs + [tf]
        mt = sandbox_wrap(mt, root, sbx_home) if sandbox else mt
        menv = dict(os.environ, MUTSU_FUDGE="1")
        try:
            mp = subprocess.run(mt, capture_output=True, text=True,
                                timeout=timeout, cwd=root, env=menv)
        except subprocess.TimeoutExpired:
            n_die += 1
            first_die = first_die or f"{rel}: timeout"
            continue
        v = tap_verdict(mp.stdout + mp.stderr, mp.returncode)
        if v == "pass":
            n_pass += 1
        elif v == "fail":
            n_fail += 1
            first_fail = first_fail or f"{rel}: {first_failing_assertion(mp.stdout + mp.stderr)}"
        else:
            n_die += 1
            first_die = first_die or f"{rel}: {first_error_line(mp.stdout + mp.stderr)}"
    if n_base == 0:
        return (name, version, "test_no_baseline",
                f"{len(tests)} test files, none pass cleanly on raku", axis)
    counts = f"base={n_base} pass={n_pass} fail={n_fail} die={n_die}"
    if n_die:
        bucket, detail = "test_die", f"{counts} | {first_die}"
    elif n_fail:
        bucket, detail = "test_fail", f"{counts} | {first_fail}"
    else:
        bucket, detail = "test_pass", counts
    return (name, version, bucket, detail, axis)


def sweep_dist(name, meta, mutsu, extra_libs, timeout, include_guts, include_native,
               sandbox, run_tests=False):
    try:
        tar_path = fetch_tarball(meta)
    except Exception as e:
        return [(name, "?", "fetch_fail", str(e)[:120], "?")]
    tmp = tempfile.mkdtemp(prefix="sweep-")
    try:
        with tarfile.open(tar_path) as tf:
            tf.extractall(tmp, filter="data")
        # dist root = the single top dir, or tmp itself
        entries = [os.path.join(tmp, e) for e in os.listdir(tmp)]
        roots = [e for e in entries if os.path.isdir(e)]
        root = roots[0] if len(roots) == 1 and not os.path.exists(
            os.path.join(tmp, "META6.json")
        ) else tmp
        meta6 = os.path.join(root, "META6.json")
        if not os.path.exists(meta6):
            return [(name, "?", "no_meta6", "no META6.json", "?")]
        m6 = json.load(open(meta6))
        provides = m6.get("provides", {})
        if not provides:
            return [(name, "?", "no_provides", "empty provides", "?")]

        # signal-scan for guts/native (skip unless asked)
        src = ""
        for dirpath, _, files in os.walk(root):
            for f in files:
                if SRC_RE.search(f):
                    try:
                        src += open(os.path.join(dirpath, f), encoding="utf8",
                                    errors="ignore").read()
                    except Exception:
                        pass
        if DEEP.search(src) and not include_guts:
            return [(name, m6.get("version", "?"), "skip_guts", "deep_guts signal", "guts")]
        if NATIVE.search(src) and not include_native:
            return [(name, m6.get("version", "?"), "skip_native", "nativecall signal", "native")]
        axis = "guts" if DEEP.search(src) else ("native" if NATIVE.search(src) else "pure")

        libs = ["-I", os.path.join(root, "lib")]
        for e in extra_libs:
            libs += ["-I", e]
        sbx_home = os.path.join(tmp, "sbxhome")
        os.makedirs(sbx_home, exist_ok=True)
        rows = []
        for module in sorted(provides):
            base = [mutsu] + libs + ["-e", f"use {module}"]
            cmd = sandbox_wrap(base, root, sbx_home) if sandbox else base
            try:
                p = subprocess.run(cmd, capture_output=True, text=True,
                                   timeout=timeout, cwd=root)
                out = (p.stdout + p.stderr)
                bucket, detail = classify(module, name, p.returncode, out)
            except subprocess.TimeoutExpired:
                bucket, detail = "timeout", "timed out"
            rows.append((name, m6.get("version", "?"), bucket, f"{module}: {detail}"
                         if detail else module, axis))
            if bucket != "load_ok":
                break  # first failure represents the dist
        # If every provided module loaded, optionally run the dist's own test
        # suite (raku baseline) to measure real compatibility beyond "loads".
        if run_tests and rows and all(r[2] == "load_ok" for r in rows):
            rows.append(test_dist(name, m6.get("version", "?"), axis, root, mutsu,
                                  libs, timeout, sandbox, sbx_home))
        return rows
    finally:
        subprocess.run(["rm", "-rf", tmp])


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--n", type=int, default=100)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--timeout", type=int, default=25)
    ap.add_argument("--include-guts", action="store_true")
    ap.add_argument("--include-native", action="store_true")
    ap.add_argument("--site-repo", help="a HOME dir whose site repo has deps installed")
    ap.add_argument("--extra-lib", action="append", default=[])
    ap.add_argument("--only", action="append", default=[], help="sweep just these dists")
    ap.add_argument("--run-tests", action="store_true",
                    help="for load_ok dists, also run their test suite (raku baseline) "
                         "and bucket mutsu as test_pass/test_fail/test_die")
    ap.add_argument("--tsv", default="tmp/dist-compat-sweep.tsv")
    ap.add_argument("--sandbox", choices=["auto", "bwrap", "none"], default="auto",
                    help="confine each dist run (default auto = bwrap if present). "
                         "'none' runs untrusted dist code UNSANDBOXED — avoid.")
    args = ap.parse_args()

    sandbox = args.sandbox != "none"
    if sandbox and not have_bwrap():
        if args.sandbox == "bwrap":
            sys.exit("bwrap not found; install bubblewrap or pass --sandbox none")
        print("warning: bwrap not found — running WITHOUT a sandbox (untrusted "
              "dist code executes unconfined). Install bubblewrap.", file=sys.stderr)
        sandbox = False
    print(f"sandbox: {'bwrap (no net, ro-fs, throwaway HOME)' if sandbox else 'NONE'}",
          file=sys.stderr)

    mutsu = os.environ.get("MUTSU_BIN", "target/release/mutsu")
    if not os.path.exists(mutsu):
        sys.exit(f"mutsu binary not found: {mutsu} (build it or set MUTSU_BIN)")
    mutsu = os.path.abspath(mutsu)  # runs are cwd'd into each dist root

    extra_libs = [os.path.abspath(e) for e in args.extra_lib]
    env_home = None
    if args.site_repo:
        env_home = os.path.abspath(args.site_repo)
        os.environ["HOME"] = env_home  # so mutsu's default site repo resolves there

    best = load_index()
    if args.only:
        sample = [n for n in args.only if n in best]
        missing = [n for n in args.only if n not in best]
        for n in missing:
            print(f"warning: {n} not in index", file=sys.stderr)
    else:
        import random
        names = sorted(best)
        random.seed(args.seed)
        sample = random.sample(names, min(args.n, len(names)))

    os.makedirs(os.path.dirname(args.tsv) or ".", exist_ok=True)
    cat = collections.Counter()
    all_rows = []
    for i, n in enumerate(sample, 1):
        rows = sweep_dist(n, best[n], mutsu, extra_libs, args.timeout,
                          args.include_guts, args.include_native, sandbox,
                          run_tests=args.run_tests)
        # The load verdict is the last non-test row; a test row (if any) is extra.
        load_rows = [r for r in rows if not r[2].startswith("test_")]
        final = load_rows[-1] if load_rows else rows[-1]
        cat[final[2]] += 1
        all_rows.extend(rows)
        test_note = ""
        if args.run_tests:
            trow = next((r for r in rows if r[2].startswith("test_")), None)
            if trow:
                test_note = f"  [{trow[2]}]"
        print(f"[{i}/{len(sample)}] {n:40} -> {final[2]}  {final[3][:55]}{test_note}",
              file=sys.stderr)

    with open(args.tsv, "w") as f:
        f.write("dist\tversion\tbucket\tdetail\taxis\n")
        for r in all_rows:
            f.write("\t".join(str(x) for x in r) + "\n")

    print(f"\n=== dist-compat-sweep: {len(sample)} dists (seed {args.seed}) ===")
    order = ["load_ok", "missing_dep", "parse_error", "runtime_error", "panic",
             "timeout", "skip_guts", "skip_native", "no_meta6", "no_provides",
             "fetch_fail"]
    for k in order:
        if cat[k]:
            print(f"  {k:14} {cat[k]:3}  {100*cat[k]/len(sample):.0f}%")

    # Test-suite axis summary (only when --run-tests): of the dists that LOAD,
    # how many actually pass their own test suite (raku baseline).
    if args.run_tests:
        test_rows = [r for r in all_rows if r[2].startswith("test_")]
        tcat = collections.Counter(r[2] for r in test_rows)
        graded = tcat["test_pass"] + tcat["test_fail"] + tcat["test_die"]
        print(f"\n=== test-suite axis: {len(test_rows)} load_ok dists ran tests ===")
        for k in ("test_pass", "test_fail", "test_die", "test_no_baseline", "test_notest"):
            if tcat[k]:
                pct = f"  {100*tcat[k]/graded:.0f}% of graded" if graded and k in (
                    "test_pass", "test_fail", "test_die") else ""
                print(f"  {k:16} {tcat[k]:3}{pct}")
        if graded:
            print(f"  -> of dists with a raku baseline, {100*tcat['test_pass']/graded:.0f}% "
                  f"pass their suite on mutsu ({tcat['test_pass']}/{graded})")

    # The list of dists that pass the load smoke-test, for the dashboard's
    # "load_ok — proven to load on mutsu" section (the passing-dist report).
    # The final row per dist is authoritative (first-failure or final load_ok).
    # Test rows carry a `test_*` bucket; exclude them so the load verdict wins.
    final_bucket = {}
    for name, _ver, bucket, _detail, _axis in all_rows:
        if bucket.startswith("test_"):
            continue
        final_bucket[name] = bucket
    load_ok = sorted(n for n, b in final_bucket.items() if b == "load_ok")
    if load_ok:
        print(f"\n=== load_ok ({len(load_ok)}) — proven to load on mutsu ===")
        print(" ".join(load_ok))

    print(f"\nTSV: {args.tsv}")


if __name__ == "__main__":
    main()
