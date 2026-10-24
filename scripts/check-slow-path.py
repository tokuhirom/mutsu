#!/usr/bin/env python3
"""Ratchet on the slow-path / tree-walk-era fallbacks (#11510).

AGENTS.md bans *new* slow-path fallbacks and calls the existing ones debt. This
script makes the debt measurable: two kinds of site are counted per file and
may only go down.

  todo   `TODO: compile to bytecode` comments -- the sanctioned label for a
         temporary slow path that cannot be avoided yet. A new marker is a new
         slow path; add the bytecode instead.
  vm     `call_method_with_values(` call sites under src/vm/ -- the VM bridging
         back into the runtime's by-name dispatcher instead of dispatching
         through a compiled row, shape or call-site lane.

Counts live in scripts/slow-path-baseline.txt (`<kind> <path> <count>`). A site
above its file's count, or in a file not listed, fails. A count that falls does
NOT fail and needs no edit: a shrinking PR never conflicts with another on the
shared list. Re-cut the list when convenient (it only shrinks):

    scripts/check-slow-path.py --update
    scripts/check-slow-path.py --self-test

The dynamic counterpart is scripts/slow-path-survey.sh: the number of distinct
`Class.method` shapes that still reach `run_instance_method` over `t/` and the
roast whitelist.

Comments and string literals are masked for the call-site kind (the `todo`
kind reads comments on purpose), and `#[cfg(test)]` items are blanked, the way
scripts/check-interp-construction.py does it.
"""
from __future__ import annotations

import importlib.util
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SRC = ROOT / "src"
BASELINE = ROOT / "scripts" / "slow-path-baseline.txt"

_spec = importlib.util.spec_from_file_location(
    "panic_surface", ROOT / "scripts" / "check-panic-surface.py"
)
assert _spec and _spec.loader
_ps = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_ps)
_ps.CFG_TEST_ATTR_RE = re.compile(r"#\[cfg\((?:test|all\(test\b[^\]]*\))\)\]")

TODO_RE = re.compile(r"TODO:\s*compile to bytecode")
# `vm_call_method_with_values(` and friends are other functions: the name must
# not continue an identifier on the left.
CALL_RE = re.compile(r"(?<![A-Za-z0-9_])call_method_with_values\s*\(")

HEADER = """\
# Slow-path ratchet baseline (see scripts/check-slow-path.py, #11510).
# Format: <kind> <path> <count>, kind = todo | vm. Counts may only go down; a
# fall needs no edit here. Re-cut with scripts/check-slow-path.py --update.
"""


def count_todo(src: str) -> int:
    return len(TODO_RE.findall(_ps.blank_test_regions(src)))


def count_vm_calls(src: str) -> int:
    return len(CALL_RE.findall(_ps.blank_test_regions(_ps.mask_non_code(src))))


def current_counts() -> dict[tuple[str, str], int]:
    skip = _ps.test_only_module_files()
    counts: dict[tuple[str, str], int] = {}
    for path in sorted(SRC.rglob("*.rs")):
        if path.resolve() in skip:
            continue
        rel = path.relative_to(ROOT).as_posix()
        text = path.read_text(encoding="utf-8")
        n = count_todo(text)
        if n:
            counts[("todo", rel)] = n
        if rel.startswith("src/vm/"):
            n = count_vm_calls(text)
            if n:
                counts[("vm", rel)] = n
    return counts


def read_baseline() -> dict[tuple[str, str], int]:
    counts: dict[tuple[str, str], int] = {}
    for line in BASELINE.read_text(encoding="utf-8").splitlines():
        body = line.split("#", 1)[0].strip()
        if not body:
            continue
        kind, path, n = body.split()[:3]
        counts[(kind, path)] = int(n)
    return counts


def write_baseline(counts: dict[tuple[str, str], int]) -> None:
    lines = [HEADER]
    width = max((len(p) for _, p in counts), default=0)
    for kind, path in sorted(counts):
        lines.append(f"{kind:<4} {path.ljust(width)} {counts[(kind, path)]:>3}")
    BASELINE.write_text("\n".join(lines) + "\n", encoding="utf-8")


def self_test() -> int:
    todo = """
// TODO: compile to bytecode
fn a() {} // TODO:  compile to bytecode, later
/// not a marker: TODO compile
#[cfg(test)]
mod tests {
    // TODO: compile to bytecode
}
"""
    got = count_todo(todo)
    if got != 2:
        print(f"check-slow-path: self-test (todo) expected 2, got {got}", file=sys.stderr)
        return 1
    calls = """
fn a(&mut self) { self.call_method_with_values(t, "x", vec![]); }
fn b(&mut self) { let r = i.call_method_with_values (t, "y", args)?; }
fn c(&mut self) { self.vm_call_method_with_values(t, "z", args); }
fn d(&mut self) { self.try_call_method_with_values(t, "z", args); }
// self.call_method_with_values(t, "comment", vec![]);
fn e() { let s = "call_method_with_values("; }
#[cfg(test)]
mod tests {
    fn f(&mut self) { self.call_method_with_values(t, "t", vec![]); }
}
"""
    got = count_vm_calls(calls)
    if got != 2:
        print(f"check-slow-path: self-test (vm) expected 2, got {got}", file=sys.stderr)
        return 1
    print("check-slow-path: self-test ok")
    return 0


def main(argv: list[str]) -> int:
    if "--self-test" in argv:
        return self_test()
    current = current_counts()
    allowed = read_baseline()
    if "--update" in argv:
        grown = {k: n for k, n in current.items() if n > allowed.get(k, 0)}
        if grown:
            for (kind, path), n in sorted(grown.items()):
                print(f"check-slow-path: {kind} {path} has {n} (allowed {allowed.get((kind, path), 0)});"
                      " --update only shrinks the list", file=sys.stderr)
            return 1
        write_baseline(current)
        print(f"slow-path baseline re-cut: {sum(current.values())} sites in {len(current)} rows")
        return 0

    failed = False
    for key in sorted(current):
        n, a = current[key], allowed.get(key, 0)
        if n > a:
            failed = True
            kind, path = key
            what = ("`TODO: compile to bytecode` marker(s)" if kind == "todo"
                    else "`call_method_with_values(` call site(s) in src/vm/")
            print(f"check-slow-path: {path}: {n} {what}, allowed {a}", file=sys.stderr)
    if failed:
        print(__doc__.split("\n\n", 2)[1], file=sys.stderr)
        print("\n  Implement the feature in the compiler and the VM instead of adding a slow path"
              " (AGENTS.md, Hard rules).", file=sys.stderr)
        return 1
    print(f"check-slow-path: {sum(current.values())} sites in {len(current)} rows "
          f"(baseline {sum(allowed.values())})")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
