#!/usr/bin/env python3
"""Progress report for ADR-11276's migration of built-in methods into rows.

Prints, for the working tree:

  * the quoted-name arms still in the dispatch cascades, per layer and file;
  * the registered rows, per owner group and handler kind;
  * the methods Rakudo declares on an owner (`rakudo_method_tables.txt`, the
    snapshot introspection reads, ADR-14032) that have no registered row there, per
    owner group.

It is a report, not a gate: ADR-11276 §9 records why a shared counter that every
parallel PR must keep equal coupled unrelated PRs and was dropped. Slices end when
this report shows zero arms for their owners (ADR §10.3 rule 6).

    scripts/method-rows-report.py                 # arms + rows (runs the unit-test dump)
    scripts/method-rows-report.py --arms          # only the cascade arms (no build)
    scripts/method-rows-report.py --arms --list   # ... and every arm that names a built-in method
    scripts/method-rows-report.py --rows-from F   # reuse a saved dump instead of running cargo
    scripts/method-rows-report.py --dump-rows     # print the raw `ROW` lines and exit
    scripts/method-rows-report.py --inventory collections,"quant hashes"   # a slice's inventory

The row dump is the ignored unit test `method_table::tests::dump_rows`.
"""

import argparse
import collections
import glob
import os
import re
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# ADR-11276 §10.2: the owner groups, one per slice.
GROUPS = {
    "numbers": "Int Num Rat FatRat Complex Bool",
    "text": "Str Cool Uni Blob Buf Version",
    "collections": "Any List Array Hash Map Range Seq Pair Capture Junction Nil Iterable",
    "quant hashes": "Set SetHash Bag BagHash Mix MixHash",
    "time": "Date DateTime Instant Duration",
    "match": "Match",
    "I/O": "IO::Path IO::Handle IO::Path::Parts",
    "objects": (
        "Mu Code Backtrace Backtrace::Frame Exception X::AdHoc Failure Signature "
        "CallFrame Attribute Supply X::TypeCheck::Assignment CX::Warn"
    ),
}
OWNER_GROUP = {owner: group for group, owners in GROUPS.items() for owner in owners.split()}


def group_of(owner):
    if owner.startswith("RakuAST"):
        return "RakuAST"
    return OWNER_GROUP.get(owner, "other")


ARM = re.compile(r'^\s*((?:"[^"\n]+"\s*\|\s*)*"[^"\n]+")\s*(?:if\b[^\n]*?)?(?:=>|$)')

LAYERS = [
    ("builtins pure arity cascades", ["src/builtins/methods_0arg/*.rs", "src/builtins/methods_narg/*.rs"]),
    ("runtime slow path", ["src/runtime/methods*.rs", "src/runtime/methods*/**/*.rs"]),
    ("VM mutation helpers", ["src/vm/vm_call_method_mut*.rs"]),
]


def arms_in(path, with_lines=False):
    """The `"name" | "other" => ...` arms of one file, each as its list of names
    (with its line number when `with_lines`)."""
    arms = []
    with open(path, encoding="utf-8") as handle:
        for number, line in enumerate(handle, 1):
            match = ARM.match(line)
            if match and ("=>" in line or line.rstrip().endswith(("|", "=>"))):
                names = re.findall(r'"([^"\n]+)"', match.group(1))
                arms.append((number, names) if with_lines else names)
    return arms


def method_names():
    """Every name a built-in method has: the Rakudo snapshot's.

    An arm none of whose names is one is not a method dispatch: a named-argument key
    (`"i" | "ignorecase"`), an encoding or a type name, a message string."""
    names = set()
    path = os.path.join(ROOT, "src/builtins/rakudo_method_tables.txt")
    for line in open(path, encoding="utf-8"):
        fields = line.rstrip("\n").split("\t")
        if not line.startswith("#") and len(fields) > 2:
            names.update(fields[2:])
    return names


def report_method_arms(list_them):
    """The arms that name a built-in method: what the campaign still has to move."""
    known = method_names()
    print("\n== Arms naming a built-in method (the rest are argument keys, type and encoding names) ==")
    total = 0
    for layer, patterns in LAYERS:
        files = sorted({f for p in patterns for f in glob.glob(os.path.join(ROOT, p), recursive=True)})
        count = 0
        for path in files:
            for number, names in arms_in(path, with_lines=True):
                if any(name in known for name in names):
                    count += 1
                    if list_them:
                        print(f"  {os.path.relpath(path, ROOT)}:{number}: {' | '.join(names)}")
        total += count
        print(f"{layer}: {count}")
    print(f"total: {total}")


def report_arms():
    print("== Quoted-name arms left in the cascades ==")
    total_arms, all_names = 0, set()
    for layer, patterns in LAYERS:
        files = sorted({f for p in patterns for f in glob.glob(os.path.join(ROOT, p), recursive=True)})
        per_file = {}
        for path in files:
            arms = arms_in(path)
            if arms:
                per_file[os.path.relpath(path, ROOT)] = arms
        count = sum(len(arms) for arms in per_file.values())
        distinct = {name for arms in per_file.values() for arm in arms for name in arm}
        total_arms += count
        all_names |= distinct
        print(f"\n{layer}: {count} arms, {len(distinct)} distinct names, {len(per_file)} files")
        for path, arms in sorted(per_file.items(), key=lambda item: -len(item[1]))[:12]:
            print(f"  {len(arms):4d}  {path}")
    print(f"\ntotal: {total_arms} arms, {len(all_names)} distinct names")
    return all_names


def registered_rows(args):
    if args.rows_from:
        text = open(args.rows_from, encoding="utf-8").read()
    else:
        command = ["cargo", "test", "--lib", "method_table::tests::dump_rows", "--", "--ignored", "--nocapture"]
        run = subprocess.run(command, cwd=ROOT, capture_output=True, text=True)
        if run.returncode != 0:
            sys.exit(run.stderr[-2000:] or "cargo test failed")
        text = run.stdout
    rows = []
    for line in text.splitlines():
        fields = line.split("\t")
        if fields[0] == "ROW" and len(fields) >= 7:
            rows.append(fields[1:7])
    return rows, text


def report_rows(args):
    registered, raw = registered_rows(args)
    if args.dump_rows:
        print(raw, end="")
        return
    print("\n== Registered rows (method_table) ==")
    by_group = collections.defaultdict(collections.Counter)
    for owner, _name, _arity, kind, _flags, _named in registered:
        by_group[group_of(owner)][kind] += 1
    kinds = ["Pure", "Narrow", "Named", "Interp", "Mut"]
    print(f"{'group':14} " + " ".join(f"{k:>7}" for k in kinds) + f" {'total':>7}")
    for group in sorted(by_group):
        counts = by_group[group]
        print(f"{group:14} " + " ".join(f"{counts[k]:7d}" for k in kinds) + f" {sum(counts.values()):7d}")
    print(f"total registered rows: {len(registered)}")

    have = {(owner, name) for owner, name, *_ in registered}
    print("\n== Methods Rakudo declares with no registered row at that owner ==")
    remaining = collections.Counter()
    total = 0
    for owner, names in declared_by_rakudo().items():
        for name in names:
            total += 1
            if (owner, name) not in have:
                remaining[group_of(owner)] += 1
    for group, count in sorted(remaining.items(), key=lambda item: -item[1]):
        print(f"{group:14} {count:7d}")
    print(f"remaining: {sum(remaining.values())} of {total} declared pairs")


def arm_files_by_name():
    """`name -> {cascade file}` over every quoted-name arm the layers still hold."""
    files_of = collections.defaultdict(set)
    for _layer, patterns in LAYERS:
        for path in sorted({f for p in patterns for f in glob.glob(os.path.join(ROOT, p), recursive=True)}):
            for arm in arms_in(path):
                for name in arm:
                    files_of[name].add(os.path.basename(path))
    return files_of


def declared_by_rakudo():
    """`owner -> {method names}` Rakudo declares in the type's own method table (the oracle snapshot)."""
    declared = {}
    path = os.path.join(ROOT, "src/builtins/rakudo_method_tables.txt")
    for line in open(path, encoding="utf-8"):
        fields = line.rstrip("\n").split("\t")
        if fields[0] in ("declared", "declared.e"):
            declared.setdefault(fields[1], set()).update(n for n in fields[2:] if n)
    return declared


def report_inventory(args):
    """A slice's first commit (ADR §10.4): every method Rakudo declares on its owners that has no registered row.

    `--inventory` takes owner names or group names (see GROUPS), comma-separated. The rows are
    printed per owner, then per method name, which is how a slice cuts its families: one handler
    answers a name for every owner and shape that has it, and the cascade arms for that name go.

    Only a pair Rakudo *declares* on the owner can be registered there (the unit test
    `rows_are_declared_by_rakudo` enforces it); an inherited one is served by the ancestor's row.
    """
    owners = []
    for item in args.inventory.split(","):
        owners += GROUPS[item].split() if item in GROUPS else [item]
    registered, _raw = registered_rows(args)
    have = {(owner, name) for owner, name, *_ in registered}
    files_of = arm_files_by_name()
    per_owner = collections.defaultdict(list)
    per_name = collections.defaultdict(list)
    for owner, names in sorted(declared_by_rakudo().items()):
        if owner not in owners:
            continue
        for name in sorted(names):
            if (owner, name) not in have:
                per_owner[owner].append(name)
                per_name[name].append(owner)
    print("== Inventory: Rakudo-declared methods with no row, per owner ==")
    for owner in owners:
        print(f"{owner:10} {len(per_owner[owner]):4d}")
    print(f"total {sum(len(v) for v in per_owner.values())}, {len(per_name)} distinct names")
    print("\n== Inventory: per method name (owners; cascade files with an arm) ==")
    for name, row_owners in sorted(per_name.items(), key=lambda item: (-len(item[1]), item[0])):
        print(f"{name:16} {len(row_owners):3d} {' '.join(row_owners)}  <{','.join(sorted(files_of.get(name, [])))}>")


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--arms", action="store_true", help="only the cascade arms (no build)")
    parser.add_argument("--list", action="store_true", help="with --arms: list the arms naming a built-in method")
    parser.add_argument("--rows-from", metavar="FILE", help="read the row dump from FILE instead of running cargo")
    parser.add_argument("--dump-rows", action="store_true", help="print the raw row dump and exit")
    parser.add_argument("--inventory", metavar="OWNERS", help="a slice's inventory: the unregistered rows of these owners or groups")
    args = parser.parse_args()
    if args.inventory:
        report_inventory(args)
        return
    if args.dump_rows:
        report_rows(args)
        return
    report_arms()
    report_method_arms(args.list)
    if not args.arms:
        report_rows(args)


if __name__ == "__main__":
    main()
