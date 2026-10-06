#!/usr/bin/env python3
"""Progress report for ADR-11276's migration of built-in methods into rows.

Prints, for the working tree:

  * the quoted-name arms still in the dispatch cascades, per layer and file;
  * the registered rows, per owner group and handler kind;
  * the recognition-table rows (`native_method_row_table.rs`) that still have no
    registered row, per owner group.

It is a report, not a gate: ADR-11276 §9 records why a shared counter that every
parallel PR must keep equal coupled unrelated PRs and was dropped. Slices end when
this report shows zero arms for their owners (ADR §10.3 rule 6).

    scripts/method-rows-report.py                 # arms + rows (runs the unit-test dump)
    scripts/method-rows-report.py --arms          # only the cascade arms (no build)
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


def arms_in(path):
    """The `"name" | "other" => ...` arms of one file, each as its list of names."""
    arms = []
    with open(path, encoding="utf-8") as handle:
        for line in handle:
            match = ARM.match(line)
            if match and ("=>" in line or line.rstrip().endswith(("|", "=>"))):
                arms.append(re.findall(r'"([^"\n]+)"', match.group(1)))
    return arms


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


def recognition_rows():
    """`(owner, name) -> (arity_bits, flag_bits)` from the recognition table."""
    path = os.path.join(ROOT, "src/builtins/native_method_row_table.rs")
    text = open(path, encoding="utf-8").read()
    rows = {}
    for owner, name, arity, flags in re.findall(r'\("([^"]+)",\s*"([^"]+)",\s*(\d+),\s*(\d+)\)', text):
        rows.setdefault((owner, name), (int(arity), int(flags)))
    return rows


def kind_of(arity_bits, flag_bits):
    """The planning estimate of ADR §10.2: Pure, Interp or Mut."""
    if flag_bits & 2:  # MUTATES_RECEIVER
        return "Mut"
    if flag_bits & 4 or arity_bits & 7 == 0:  # SPECIAL, or no pure arity
        return "Interp"
    return "Pure"


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
    kinds = ["Pure", "Narrow", "Named", "Interp"]
    print(f"{'group':14} " + " ".join(f"{k:>7}" for k in kinds) + f" {'total':>7}")
    for group in sorted(by_group):
        counts = by_group[group]
        print(f"{group:14} " + " ".join(f"{counts[k]:7d}" for k in kinds) + f" {sum(counts.values()):7d}")
    print(f"total registered rows: {len(registered)}")

    have = {(owner, name) for owner, name, *_ in registered}
    print("\n== Recognition-table rows with no registered row ==")
    remaining = collections.defaultdict(collections.Counter)
    table = recognition_rows()
    for (owner, name), (arity_bits, flag_bits) in table.items():
        if (owner, name) not in have:
            remaining[group_of(owner)][kind_of(arity_bits, flag_bits)] += 1
    kinds = ["Pure", "Interp", "Mut"]
    print(f"{'group':14} " + " ".join(f"{k:>7}" for k in kinds) + f" {'total':>7}")
    grand = 0
    for group in sorted(remaining, key=lambda g: -sum(remaining[g].values())):
        counts = remaining[group]
        grand += sum(counts.values())
        print(f"{group:14} " + " ".join(f"{counts[k]:7d}" for k in kinds) + f" {sum(counts.values()):7d}")
    print(f"remaining: {grand} of {len(table)} recognition rows")


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
        if fields[0] == "declared":
            declared[fields[1]] = set(fields[2:])
    return declared


def report_inventory(args):
    """A slice's first commit (ADR §10.4): every recognition row of its owners that has no registered row.

    `--inventory` takes owner names or group names (see GROUPS), comma-separated. The rows are
    printed per owner, then per method name, which is how a slice cuts its families: one handler
    answers a name for every owner and shape that has it, and the cascade arms for that name go.

    Only a pair Rakudo *declares* on the owner can be registered there (the unit test
    `rows_are_declared_by_rakudo` enforces it). The others (`Array.keys`, declared on `List`)
    are `inherited`: they are served by the ancestor's row once the receiver's shape inherits it.
    """
    owners = []
    for item in args.inventory.split(","):
        owners += GROUPS[item].split() if item in GROUPS else [item]
    registered, _raw = registered_rows(args)
    have = {(owner, name) for owner, name, *_ in registered}
    declared = declared_by_rakudo()
    files_of = arm_files_by_name()
    per_owner = collections.defaultdict(list)
    per_name = collections.defaultdict(list)
    for (owner, name), (arity_bits, flag_bits) in sorted(recognition_rows().items()):
        if owner in owners and (owner, name) not in have:
            kind = kind_of(arity_bits, flag_bits)
            is_declared = name in declared.get(owner, ())
            per_owner[owner].append((name, kind, is_declared))
            per_name[name].append((owner, kind, is_declared))
    print("== Inventory: unregistered rows per owner (declared = Rakudo declares it there) ==")
    total = collections.Counter()
    for owner in owners:
        kinds = collections.Counter(kind for _name, kind, is_declared in per_owner[owner] if is_declared)
        inherited = sum(1 for _name, _kind, is_declared in per_owner[owner] if not is_declared)
        total.update(kinds)
        total["inherited"] += inherited
        print(
            f"{owner:10} declared {sum(kinds.values()):4d}  "
            + " ".join(f"{k} {kinds[k]}" for k in sorted(kinds))
            + f"   inherited-only {inherited}"
        )
    print(f"total declared {sum(v for k, v in total.items() if k != 'inherited')}, inherited-only {total['inherited']}, {len(per_name)} distinct names")
    print("\n== Inventory: per method name (declared rows, kinds, owners; inherited-only owners in brackets; cascade files with an arm) ==")
    for name, rows in sorted(per_name.items(), key=lambda item: (-len(item[1]), item[0])):
        declared_rows = [(o, k) for o, k, d in rows if d]
        inherited_owners = [o for o, _k, d in rows if not d]
        kinds = "".join(sorted({k[0] for _o, k in declared_rows})) or "-"
        owners_text = " ".join(o for o, _k in declared_rows)
        extra = f" [{' '.join(inherited_owners)}]" if inherited_owners else ""
        print(f"{name:16} {len(declared_rows):3d} {kinds:3} {owners_text}{extra}  <{','.join(sorted(files_of.get(name, [])))}>")


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--arms", action="store_true", help="only the cascade arms (no build)")
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
    if not args.arms:
        report_rows(args)


if __name__ == "__main__":
    main()
