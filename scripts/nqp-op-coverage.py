#!/usr/bin/env python3
"""nqp-op-coverage.py — which documented `nqp::` ops does mutsu implement?

Builds the op inventory the `nqp::` coverage campaign is tracked against
(docs/nqp-op-coverage.md, parent issue linked from PLAN.md):

  1. The universe is NQP's own op reference, `docs/ops.markdown` (one
     `# <a id=...></a> Category` heading per op family, one `* \\`op(...)\\``
     line per op variant), plus the Rakudo-only `p6*` ops registered in
     Rakudo's `src/vm/moar/Perl6/Ops.nqp`. Both are fetched from GitHub
     unless a local copy is given.
  2. Each op is probed as `use nqp; nqp::<op>(1, 1, ...)` with 0..5
     arguments. mutsu dies with "Unsupported nqp:: op" for a name no
     dispatch table (nor the compiler's control-op lowering) claims, so an
     op counts as implemented as soon as one arity gets any other outcome —
     a result, a type error, an arity error. That makes this a coverage
     probe, not a correctness test: an implemented op can still be wrong.
  3. Out of scope, listed separately: ops the reference marks JS- or
     JVM-only (mutsu emulates the MoarVM backend), and ops Rakudo itself
     rejects with "No registered operation handler" (NQP-only ops no Raku
     program can reach; needs `raku` on PATH). The `RUSAGE_*` / `UNAME_*`
     entries are constants and are probed as `nqp::const::NAME`.

Usage:
  scripts/nqp-op-coverage.py [--mutsu target/debug/mutsu] [--raku PATH]
                             [--ops-markdown PATH] [--rakudo-ops PATH]
                             [--format markdown|json] [--jobs N]
"""
import argparse
import collections
import concurrent.futures
import json
import re
import shutil
import subprocess
import sys
import urllib.request

OPS_URL = "https://raw.githubusercontent.com/Raku/nqp/main/docs/ops.markdown"
RAKUDO_OPS_URL = ("https://raw.githubusercontent.com/rakudo/rakudo/main/"
                  "src/vm/moar/Perl6/Ops.nqp")
RAKUDO_CATEGORY = "Rakudo p6* (HLL)"

# Ops a Raku program can name but never actually run, with the reason; they
# are reported under "Not applicable" instead of as missing. Add an entry only
# with evidence (what Rakudo itself does), never to make the table look done.
NOT_APPLICABLE = {
    "list_b": "Rakudo rejects every Raku call at compile time (\"The 'list_b' op "
              "needs a list of blocks, got QAST::Op\"): a Raku block literal "
              "never compiles to the bare QAST::Block the op requires.",
}

# Tracking issue per category (the campaign's sub-issues). Several small
# categories share one issue.
TRACKING = {
    "Arithmetic": 11490,
    "Numeric": 11490,
    "Trigonometric": 11490,
    "Bit": 11491,
    "Relational / Logic": 11491,
    "Coercion": 11553,
    "Type / Conversion": 11553,
    "Array": 11493,
    "Hash": 11494,
    "String": 11495,
    "Unicode Properties": 11495,
    "Captures": 11496,
    "Exception Handling": 11497,
    "Context Introspection": 11498,
    "Objects": 11499,
    "Parametric Extensions": 11499,
    "Miscellaneous": 11499,
    "Conditional": 11500,
    "Loop/Control": 11500,
    "Input/Output": 11501,
    "File / Directory / Network": 11501,
    "Processes": 11501,
    "System Introspection": 11501,
    "Timish": 11501,
    "Asynchronous": 11502,
    "Threads": 11502,
    "Atomic": 11502,
    "Stream Decoding": 11503,
    "Serialization context": 11504,
    "HLL-Specific": 11504,
    "Profiling": 11504,
    "NativeCall": 11504,
    RAKUDO_CATEGORY: 11505,
}


def read(path, url):
    if path:
        with open(path, encoding="utf-8") as f:
            return f.read()
    with urllib.request.urlopen(url, timeout=60) as r:
        return r.read().decode("utf-8")


def parse_ops_markdown(text):
    """Yield {cat, op, only} for each op variant, in document order."""
    cat = None
    seen = set()
    for line in text.splitlines():
        m = re.match(r'^# <a id="[^"]+"></a>\s*(.*)', line)
        if m:
            cat = m.group(1).strip()
            continue
        if cat is None:
            continue
        m = re.match(r'^\*\s+`([A-Za-z_][A-Za-z0-9_]*)\s*[(`]', line)
        if not m or (cat, m.group(1)) in seen:
            continue
        seen.add((cat, m.group(1)))
        only = re.findall(r'`(js|jvm|moar)`', line[m.end():])
        yield {"cat": cat, "op": m.group(1), "only": only}


def parse_rakudo_ops(text, documented):
    """Yield the Rakudo-only ops (those the NQP reference does not list)."""
    names = re.findall(
        r"add_(?:hll_)?(?:op|moarop_mapping)\((?:\$hll, |'Raku', )?'([A-Za-z0-9_]+)'",
        text)
    for name in sorted(set(names)):
        if name != "nqp" and name not in documented:
            yield {"cat": RAKUDO_CATEGORY, "op": name, "only": []}


def out_of_scope(o):
    """Ops no Raku program can reach, so mutsu has nothing to match.

    JS/JVM-only ops (mutsu emulates the MoarVM backend), `nqp::const`
    (reached as `nqp::const::NAME`, whose names are probed individually),
    and ops Rakudo itself rejects with "No registered operation handler"
    (NQP-only ops the Raku HLL never registered).
    """
    if o["op"].startswith("jvm") or o["op"] in ("js", "const"):
        return True
    if o["op"] in NOT_APPLICABLE:
        return True
    if o.get("raku_unreachable"):
        return True
    return bool(o["only"]) and "moar" not in o["only"]


def source(o, n):
    if re.fullmatch(r"[A-Z][A-Z0-9_]*", o["op"]):
        # RUSAGE_* / UNAME_* are constants, spelled nqp::const::NAME.
        return "use nqp; nqp::const::%s;" % o["op"]
    return "use nqp; nqp::%s(%s);" % (o["op"], ", ".join(["1"] * n))


def run(argv, code):
    try:
        p = subprocess.run(["timeout", "10"] + argv + ["-e", code],
                           capture_output=True, text=True, timeout=30)
        return p.stderr + p.stdout
    except subprocess.TimeoutExpired:
        return ""


def probe(binary, raku, o):
    o = dict(o)
    if raku:
        err = run([raku], source(o, 0))
        o["raku_unreachable"] = ("No registered operation handler" in err
                                 or "Unknown constant" in err)
    o["implemented"] = False
    for n in range(6):
        if "Unsupported nqp:: op" not in run([binary], source(o, n)):
            o["implemented"] = True
            break
    return o


def markdown(results):
    by_cat = collections.OrderedDict()
    for r in results:
        by_cat.setdefault(r["cat"], []).append(r)
    in_scope = [r for r in results if not out_of_scope(r)]
    done = sum(r["implemented"] for r in in_scope)
    lines = [
        "| Category | Implemented | Missing | Tracking |",
        "| --- | ---: | ---: | --- |",
    ]
    for cat, rs in by_cat.items():
        rs = [r for r in rs if not out_of_scope(r)]
        if not rs:
            continue
        ok = sum(r["implemented"] for r in rs)
        issue = TRACKING.get(cat)
        link = "#%d" % issue if issue else ""
        lines.append("| %s | %d / %d | %d | %s |"
                     % (cat, ok, len(rs), len(rs) - ok, link))
    lines.append("| **Total** | **%d / %d** | **%d** | |"
                 % (done, len(in_scope), len(in_scope) - done))
    lines.append("")
    lines.append("### Missing ops by category")
    lines.append("")
    for cat, rs in by_cat.items():
        missing = [r["op"] for r in rs
                   if not r["implemented"] and not out_of_scope(r)]
        if missing:
            issue = TRACKING.get(cat)
            suffix = " (#%d)" % issue if issue else ""
            lines.append("- **%s**%s: %s" % (
                cat, suffix, ", ".join("`%s`" % m for m in missing)))
    skipped = [r["op"] for r in results
               if out_of_scope(r) and r["op"] not in NOT_APPLICABLE]
    lines.append("")
    lines.append("Out of scope (JS/JVM-only, `const` as a call, or rejected "
                 "by Rakudo itself): %s"
                 % ", ".join("`%s`" % s for s in skipped))
    lines.append("")
    lines.append("## Not applicable")
    lines.append("")
    for op, reason in sorted(NOT_APPLICABLE.items()):
        lines.append("- `%s`: %s" % (op, reason))
    return "\n".join(lines) + "\n"


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--mutsu", default="target/debug/mutsu")
    ap.add_argument("--ops-markdown")
    ap.add_argument("--rakudo-ops")
    ap.add_argument("--format", choices=["markdown", "json"], default="markdown")
    ap.add_argument("--jobs", type=int, default=8)
    ap.add_argument("--raku", default=shutil.which("raku"),
                    help="Rakudo binary used to drop ops Raku cannot reach "
                         "(default: raku on PATH; pass '' to skip)")
    a = ap.parse_args()
    ops = list(parse_ops_markdown(read(a.ops_markdown, OPS_URL)))
    documented = {o["op"] for o in ops}
    ops += list(parse_rakudo_ops(read(a.rakudo_ops, RAKUDO_OPS_URL), documented))
    with concurrent.futures.ThreadPoolExecutor(a.jobs) as ex:
        results = list(ex.map(lambda o: probe(a.mutsu, a.raku, o), ops))
    if a.format == "json":
        json.dump(results, sys.stdout, indent=1)
        print()
    else:
        sys.stdout.write(markdown(results))


if __name__ == "__main__":
    main()
