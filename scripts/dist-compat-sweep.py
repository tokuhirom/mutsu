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


def sweep_dist(name, meta, mutsu, extra_libs, timeout, include_guts, include_native,
               sandbox):
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
                          args.include_guts, args.include_native, sandbox)
        final = rows[-1]  # last row = first-failure or final load_ok
        cat[final[2]] += 1
        all_rows.extend(rows)
        print(f"[{i}/{len(sample)}] {n:40} -> {final[2]}  {final[3][:70]}",
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

    # The list of dists that pass the load smoke-test, for the dashboard's
    # "load_ok — proven to load on mutsu" section (the passing-dist report).
    # The final row per dist is authoritative (first-failure or final load_ok).
    final_bucket = {}
    for name, _ver, bucket, _detail, _axis in all_rows:
        final_bucket[name] = bucket
    load_ok = sorted(n for n, b in final_bucket.items() if b == "load_ok")
    if load_ok:
        print(f"\n=== load_ok ({len(load_ok)}) — proven to load on mutsu ===")
        print(" ".join(load_ok))

    print(f"\nTSV: {args.tsv}")


if __name__ == "__main__":
    main()
