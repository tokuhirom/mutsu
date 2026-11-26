#!/usr/bin/env python3
"""Generate (or check) linker/startup-text.ld, the startup text layout (#19197).

The layout gathers the functions interpreter start-up executes into one output
section in front of `.text`, so a short program faults in a few dozen 64 KiB
windows of mutsu's code instead of ~400 (see `build.rs`).

  scripts/gen-startup-layout.py            # regenerate from target/release/mutsu
  scripts/gen-startup-layout.py --check    # how much of the file still matches

How it is generated: each workload below runs under callgrind with
`--dump-instr=yes`, every executed instruction address in the mutsu binary is
mapped to its symbol with `nm`, and each symbol becomes one input-section
pattern (`.text.<symbol>`, since rustc emits one section per function).

The patterns are hash-free so the file survives rebuilds: a v0 crate
disambiguator (`Cs<hash>_`) depends on the toolchain, the package version and
the feature set, and an `.llvm.<hash>` suffix on a promoted local symbol
changes with any edit to its codegen unit, so both become `*`. Anything else
that drifts (a renamed or removed function) only stops matching, and an
unmatched pattern is ignored by the linker. Regenerate when `--check` reports
that a large share of the patterns has stopped matching, or after a change that
moves start-up work into new functions.

Generate from an optimized build of the default features made the way CI makes
it (`CARGO_PROFILE_RELEASE_INCREMENTAL=false cargo build --release`): an
incremental build splits the crate into many more codegen units, so it carries
more per-unit copies of inlined helpers than the shipped binary.
"""

import argparse
import bisect
import fnmatch
import os
import re
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DEFAULT_BINARY = os.path.join(ROOT, "target", "release", "mutsu")
DEFAULT_OUTPUT = os.path.join(ROOT, "linker", "startup-text.ld")
OUTPUT_SECTION = ".text.mutsu_startup"

# What start-up means here: the empty program and `bench-startup`, each with
# the JIT off (the interpreter series) and in its default mode.
WORKLOADS = [
    (["-e", ""], {"MUTSU_JIT": "off"}),
    (["-e", ""], {}),
    (["benchmarks/bench-startup.raku"], {"MUTSU_JIT": "off"}),
    (["benchmarks/bench-startup.raku"], {}),
]


def text_symbols(binary):
    """(address, size, mangled name) of every defined function, sorted."""
    out = subprocess.run(
        ["nm", "-S", "--defined-only", binary], capture_output=True, text=True, check=True
    ).stdout
    syms = []
    for line in out.splitlines():
        parts = line.split()
        if len(parts) == 4 and parts[2] in "tTwW":
            syms.append((int(parts[0], 16), int(parts[1], 16), parts[3]))
    syms.sort()
    return syms


def executed_addresses(binary, cg_out, main_addr):
    """Instruction addresses executed inside `binary`, from a callgrind dump,
    as addresses in the binary's own (`nm`) address space."""
    target = os.path.realpath(binary)
    named = set()
    unnamed = set()
    main_min = None
    ob = None
    fn = None
    with open(cg_out) as f:
        for line in f:
            if line.startswith("ob="):
                ob = line[3:].strip()
            elif line.startswith("fn="):
                fn = line[3:].strip()
            elif line.startswith("0x"):
                addr = int(line.split(None, 1)[0], 16)
                if ob == "???":
                    unnamed.add(addr)
                    if fn == "main":
                        main_min = addr if main_min is None else min(main_min, addr)
                elif ob is not None and os.path.realpath(ob) == target:
                    named.add(addr)
    # Valgrind does not recognise a binary whose executable segment holds more
    # than `.text` -- i.e. one already built with this layout -- as an object:
    # its code comes out under `ob=???` at absolute (load-biased) addresses.
    # The C `main`'s entry gives the bias. Other `???` code (JIT output, the
    # vDSO) maps to no symbol and is dropped by the caller.
    if main_min is not None:
        bias = main_min - main_addr
        named |= {a - bias for a in unnamed}
    return named


def run_workloads(binary, main_addr):
    addrs = set()
    os.makedirs(os.path.join(ROOT, "tmp"), exist_ok=True)
    with tempfile.TemporaryDirectory(dir=os.path.join(ROOT, "tmp")) as tmp:
        for i, (args, extra_env) in enumerate(WORKLOADS):
            env = dict(os.environ, **extra_env)
            cmd = [binary, *args]
            # Warm the module precompilation cache first (perf-tuning skill §0).
            subprocess.run(cmd, env=env, cwd=ROOT, stdout=subprocess.DEVNULL, check=True)
            cg_out = os.path.join(tmp, f"cg.{i}.out")
            subprocess.run(
                [
                    "valgrind",
                    "--tool=callgrind",
                    "--dump-instr=yes",
                    "--compress-pos=no",
                    "--compress-strings=no",
                    "--cache-sim=no",
                    "--branch-sim=no",
                    f"--callgrind-out-file={cg_out}",
                    *cmd,
                ],
                env=env,
                cwd=ROOT,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                check=True,
            )
            addrs |= executed_addresses(binary, cg_out, main_addr)
    return addrs


def pattern_for(symbol):
    """The hash-free input-section pattern that matches `symbol`'s section."""
    suffix = ""
    m = re.fullmatch(r"(.*)\.llvm\.\d+", symbol)
    if m:
        symbol, suffix = m.group(1), "*"
    if symbol.startswith("_R"):
        symbol = re.sub(r"Cs[0-9A-Za-z]+_", "Cs*_", symbol)
    return ".text." + symbol + suffix


def read_patterns(path):
    pats = []
    with open(path) as f:
        for line in f:
            m = re.fullmatch(r"\s*\*\((\S+)\)\s*", line)
            if m:
                pats.append(m.group(1))
    return pats


def generate(args):
    syms = text_symbols(args.binary)
    starts = [s[0] for s in syms]
    main_addr = next(a for a, _, name in syms if name == "main")
    executed = set()
    for addr in run_workloads(args.binary, main_addr):
        i = bisect.bisect_right(starts, addr) - 1
        if i >= 0 and syms[i][0] <= addr < syms[i][0] + max(syms[i][1], 1):
            executed.add(syms[i][2])
    pats = sorted({pattern_for(s) for s in executed})
    with open(args.output, "w") as o:
        o.write(
            "/* Startup text layout of the mutsu binary (#19197).\n"
            " *\n"
            " * GENERATED by scripts/gen-startup-layout.py -- do not edit by hand.\n"
            " * Applied by build.rs, which explains what it is for. Each line names\n"
            " * the section of one function that interpreter start-up executes;\n"
            " * crate-hash parts are wildcards, and a line that matches nothing is\n"
            " * ignored by the linker.\n"
            " */\n"
            "SECTIONS {\n"
            f"  {OUTPUT_SECTION} : {{\n"
        )
        for p in pats:
            o.write(f"    *({p})\n")
        o.write("  }\n}\nINSERT BEFORE .text;\n")
    print(f"{len(executed)} executed functions -> {len(pats)} patterns in {args.output}")


def check(args):
    pats = read_patterns(args.output)
    sizes = {}
    for _, size, name in text_symbols(args.binary):
        key = ".text." + name
        sizes[key] = sizes.get(key, 0) + size
    names = sorted(sizes)
    # Match each pattern only against the names that share its literal prefix
    # (everything before the first `*`), found by binary search.
    matched = 0
    matched_bytes = 0
    for p in pats:
        lit = p.split("*", 1)[0]
        hits = []
        for n in names[bisect.bisect_left(names, lit) :]:
            if not n.startswith(lit):
                break
            if fnmatch.fnmatchcase(n, p):
                hits.append(n)
        if hits:
            matched += 1
            matched_bytes += sum(sizes[h] for h in hits)
    share = matched / len(pats) if pats else 0.0
    print(
        f"{matched}/{len(pats)} patterns ({share:.0%}) match a function in {args.binary}, "
        f"{matched_bytes / 1e6:.2f} MB of code"
    )
    return 0 if share >= args.min_share else 1


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--binary", default=DEFAULT_BINARY)
    ap.add_argument("--output", default=DEFAULT_OUTPUT)
    ap.add_argument("--check", action="store_true", help="report how many patterns still match")
    ap.add_argument(
        "--min-share",
        type=float,
        default=0.8,
        help="--check exits 1 below this share of matching patterns (default 0.8)",
    )
    args = ap.parse_args()
    if args.check:
        return check(args)
    generate(args)
    return 0


if __name__ == "__main__":
    sys.exit(main())
