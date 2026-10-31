#!/usr/bin/env python3
"""The `.AST` text ratchet (RakuAST roadmap #7564, C1 / #15759).

usage: ratchet.py golden RAKUDO_DIR SAMPLE_LIST GOLDEN
       ratchet.py check   MUTSU_DIR GOLDEN IDENTICAL
       ratchet.py update  MUTSU_DIR GOLDEN IDENTICAL

`GOLDEN` (ci/ast-text-corpus/rakudo.tsv) is what rakudo's `.AST` text of a pinned
sample of roast files hashes to: a `F <TAB> file <TAB> sha1 of the file` line per
file, then an `S <TAB> statement index <TAB> sha1[:12] of the .raku text` line per
top-level statement of it.

CI has no `raku`, so the rakudo side is these hashes, made once with
`scripts/ast-text-corpus.sh golden` on the rakudo release mutsu tracks (vendor.lock).
`IDENTICAL` (ci/ast-text-corpus/identical.tsv) lists the statements whose mutsu
text equalled rakudo's when it was last updated: `check` fails when one no longer
does, `update` adds the ones that do now and never removes one. A file whose
content changed since the golden was made (a re-vendored roast) is stale: it is
skipped and reported until the golden is made again.
"""
import hashlib
import os
import sys


def blocks(path):
    """`{index: text}` of a dump written by `ast-text-corpus.sh run`."""
    result, current = {}, None
    if not os.path.exists(path):
        return result
    with open(path, errors="replace") as handle:
        for line in handle:
            if line.startswith("@@@ "):
                current = int(line[4:])
                result[current] = []
            elif current is not None:
                result[current].append(line.rstrip("\n"))
    return {i: "\n".join(lines) for i, lines in result.items()}


def digest(text):
    return hashlib.sha1(text.encode("utf-8", "replace")).hexdigest()


def file_digest(path):
    with open(path, "rb") as handle:
        return hashlib.sha1(handle.read()).hexdigest()


def dump_name(file):
    return file.replace("/", "_") + ".out"


def read_golden(path):
    golden, current = {}, None
    with open(path) as handle:
        for line in handle:
            if line.startswith("#") or not line.strip():
                continue
            kind, a, b = line.rstrip("\n").split("\t")
            if kind == "F":
                current = golden.setdefault(a, (b, {}))[1]
            else:
                current[int(a)] = b
    return golden


def read_identical(path):
    out = set()
    if os.path.exists(path):
        with open(path) as handle:
            for line in handle:
                if line.startswith("#") or not line.strip():
                    continue
                file, idx = line.rstrip("\n").split("\t")
                out.add((file, int(idx)))
    return out


def write_identical(path, entries):
    with open(path, "w") as handle:
        handle.write(
            "# Statements whose `.AST` text equals rakudo's (scripts/ast-text-corpus.sh update).\n"
            "# Only ever grows: scripts/ast-text-corpus.sh check fails when one stops matching.\n"
        )
        for file, idx in sorted(entries):
            handle.write(f"{file}\t{idx}\n")


def main():
    mode = sys.argv[1]
    if mode == "golden":
        rakudo_dir, sample, out = sys.argv[2:5]
        rows = 0
        with open(sample) as files, open(out, "w") as handle:
            handle.write(
                "# rakudo's `.AST` text, hashed: F file sha1-of-file, then S index sha1[:12]-of-text per statement.\n"
                "# Regenerate with scripts/ast-text-corpus.sh golden on the rakudo release in vendor.lock.\n"
            )
            for file in sorted(f.strip() for f in files if f.strip()):
                statements = blocks(os.path.join(rakudo_dir, dump_name(file)))
                if not statements:
                    continue
                handle.write(f"F\t{file}\t{file_digest(file)}\n")
                for idx in sorted(statements):
                    handle.write(f"S\t{idx}\t{digest(statements[idx])[:12]}\n")
                    rows += 1
        print(f"golden: {rows} statements")
        return 0
    mutsu_dir, golden_path, identical_path = sys.argv[2:5]
    golden = read_golden(golden_path)
    # The golden stores 12 hex digits; compare on the same prefix.
    now, total, stale, missing = measure(mutsu_dir, golden)
    listed = read_identical(identical_path)
    share = 100.0 * len(now) / total if total else 0.0
    print(f"ast-text: {len(now)} of {total} statements identical to rakudo ({share:.1f}%) in {len(golden)} files")
    if stale:
        print(f"ast-text: {len(stale)} file(s) changed since the golden was made and are skipped: {', '.join(stale[:5])}")
    if missing:
        print(f"ast-text: {len(missing)} file(s) no longer exist and are skipped: {', '.join(missing[:5])}")
    if mode == "check":
        stale_set = set(stale) | set(missing)
        regressed = sorted(e for e in listed if e not in now and e[0] not in stale_set)
        if regressed:
            print(f"ast-text: {len(regressed)} statement(s) listed in {identical_path} no longer match rakudo:", file=sys.stderr)
            for file, idx in regressed[:40]:
                print(f"  {file} statement {idx}", file=sys.stderr)
            print("  (scripts/ast-text-corpus.sh show CLASS prints the differing hunks)", file=sys.stderr)
            return 1
        gained = len(now - listed)
        if gained:
            print(f"ast-text: {gained} statement(s) now match and are not listed yet: scripts/ast-text-corpus.sh update")
        return 0
    if mode == "update":
        keep = {e for e in listed if e[0] not in set(missing)}
        write_identical(identical_path, keep | now)
        print(f"ast-text: {identical_path} now lists {len(keep | now)} statements")
        return 0
    return 2


def measure(mutsu_dir, golden):
    identical, total, stale, missing = set(), 0, [], []
    for file, (fsha, statements) in sorted(golden.items()):
        if not os.path.exists(file):
            missing.append(file)
            continue
        if file_digest(file) != fsha:
            stale.append(file)
            continue
        mutsu = blocks(os.path.join(mutsu_dir, dump_name(file)))
        for idx, ssha in statements.items():
            total += 1
            if idx in mutsu and digest(mutsu[idx])[:12] == ssha:
                identical.add((file, idx))
    return identical, total, stale, missing


sys.exit(main())
