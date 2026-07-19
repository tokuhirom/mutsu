#!/usr/bin/env python3
"""
ecosystem-guts-survey.py — estimate how much of the Raku (fez) ecosystem needs a
compiler-guts layer (NQP / QAST / slang / macros) that mutsu deliberately does
not implement, versus how much is reachable by improving pure-Raku compatibility.

Method: read the local fez ecosystem index (the same one `mzef`/zef downloads),
take the latest version of each distribution, draw a fixed-seed random sample,
download each dist tarball from the fez CDN, and scan its Raku sources for a set
of signals. Each dist is placed in exactly ONE bucket by decreasing severity:

  deep_guts    QAST / Metamodel::Primitives / NQPHLL / :from<NQP> / EXPORTHOW /
               define_slang / package_declarator:sym / experimental :macros /
               Slang::   -> genuinely needs the compiler-guts layer (Test::Async
               class). Not reachable without an NQP/slang subsystem.
  nativecall   use NativeCall / is native / :from<C>  -> needs the C FFI layer,
               which mutsu HAS as an MVP. Partially reachable; NOT NQP-blocked.
  nqp_ops_only use nqp / nqp::  (but none of the deep signals) -> a spectrum;
               many nqp ops are simple and stubbable / perf-only.
  pure_raku    none of the above -> not guts-blocked (does not by itself prove
               mutsu runs it, only that no guts layer is required).

Caveats: fez ecosystem only (not the older github/cpan ecosystems); a signal
scan, not execution; "pure_raku" means "not guts-blocked", not "runs on mutsu".

Usage:
  scripts/ecosystem-guts-survey.py [SAMPLE_SIZE] [SEED]
  # defaults: SAMPLE_SIZE=150 SEED=42 ; needs ~/.zef/store/fez/fez.json present
  # (populate it once with any `mzef`/zef ecosystem operation).
"""
import json, os, re, sys, io, random, tarfile, urllib.request, collections

FEZ_INDEX = os.path.expanduser("~/.zef/store/fez/fez.json")
FEZ_CDN = "https://360.zef.pm/"

DEEP = re.compile(
    r"QAST|Metamodel::Primitives|NQPHLL|:from<NQP>|EXPORTHOW|define_slang|"
    r"package_declarator:sym|use\s+experimental\s*:\s*macro|Slang::"
)
NQP = re.compile(r"\buse\s+nqp\b|nqp::")
NATIVE = re.compile(r"\buse\s+NativeCall\b|:from<C>|\bis\s+native\b")
SRC_RE = re.compile(r"\.(rakumod|pm6|pm|raku|rakutest|t)$")


def version_key(v):
    return [(0, int(x)) if x.isdigit() else (1, x) for x in re.split(r"[.\-+]", str(v))]


def main():
    size = int(sys.argv[1]) if len(sys.argv) > 1 else 150
    seed = int(sys.argv[2]) if len(sys.argv) > 2 else 42
    if not os.path.exists(FEZ_INDEX):
        sys.exit(f"missing {FEZ_INDEX} — run an mzef/zef ecosystem op first")

    idx = json.load(open(FEZ_INDEX))
    best = {}
    for m in idx:
        if isinstance(m, dict) and m.get("name") and m.get("path"):
            n = m["name"]
            if n not in best or version_key(m.get("version", "0")) > version_key(
                best[n].get("version", "0")
            ):
                best[n] = m

    names = sorted(best)
    random.seed(seed)
    sample = random.sample(names, min(size, len(names)))

    cat = collections.Counter()
    deep = []
    scanned = 0
    for n in sample:
        try:
            data = urllib.request.urlopen(FEZ_CDN + best[n]["path"], timeout=40).read()
            tf = tarfile.open(fileobj=io.BytesIO(data))
            src = ""
            for mem in tf.getmembers():
                if mem.isfile() and SRC_RE.search(mem.name):
                    try:
                        src += tf.extractfile(mem).read().decode("utf8", "ignore")
                    except Exception:
                        pass
            scanned += 1
            if DEEP.search(src):
                cat["deep_guts"] += 1
                deep.append(n)
            elif NATIVE.search(src):
                cat["nativecall"] += 1
            elif NQP.search(src):
                cat["nqp_ops_only"] += 1
            else:
                cat["pure_raku"] += 1
        except Exception:
            cat["fetch_fail"] += 1

    print(f"unique dists in index: {len(names)}")
    print(f"sampled: {len(sample)}  scanned: {scanned}  seed: {seed}\n")
    for k in ("pure_raku", "nativecall", "nqp_ops_only", "deep_guts"):
        pct = 100 * cat[k] / scanned if scanned else 0
        print(f"  {k:14} {cat[k]:3}/{scanned}  {pct:.0f}%")
    print(f"\ndeep_guts (genuinely compiler-guts blocked): {deep}")


if __name__ == "__main__":
    main()
