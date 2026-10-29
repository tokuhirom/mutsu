#!/usr/bin/env python3
"""Ratchet on the fixed frame names outside the frame record (ADR-14843).

A method frame's `self`, `?CLASS` and `?ROLE` live in its `FrameFixtures`
record (`routine_stack`), and a closure carries its creator's record
(`SubData::fixtures`). No `Env` key stores them: a reader asks the record
(`frame_invocant`, `frame_fixture_value`), and a by-name store of `self`
reaches the record (`store_invocant_by_name`). This script keeps it that way.

It counts, per file under the runtime layers, every spelling of the three
names in code: the string literals `"self"` / `"?CLASS"` / `"?ROLE"` and the
well-known symbols `wk::self_()`, `wk::class_decl()`, `wk::role_decl()`.
Comments and `#[cfg(test)]` code are not counted. Most remaining references are
benign name tests (`matches!(name, "self" | ...)`, the compiler-facing parameter
lists) but each one is a place that could grow an `Env` read or write of a
fixed name, so a new reference, or a new file, fails until a reviewer has
looked at it and the baseline names it:

    scripts/check-frame-fixtures.py              # check
    scripts/check-frame-fixtures.py --update     # re-cut after removing a reference
    scripts/check-frame-fixtures.py --self-test

Counts may only go down. The parser, AST, compiler and RakuAST layers are out
of scope: they spell the names as source-level syntax, not as `Env` keys.

The one `Env` key that remains is the `self` of a plain routine's own `$self`
parameter (`sub f($self) { ... }`, ADR-0061), a user lexical that shares the
spelling; it is bound by `bind_param_value_sym` in `src/runtime/types/mod.rs`.
"""
from __future__ import annotations

import importlib.util
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SRC = ROOT / "src"
BASELINE = ROOT / "scripts" / "frame-fixtures-baseline.txt"
LAYERS = ("runtime", "vm", "builtins", "value", "gc", "jit")
ROOT_FILES = ("env.rs", "symbol.rs", "interpreter.rs")

_spec = importlib.util.spec_from_file_location(
    "panic_surface", ROOT / "scripts" / "check-panic-surface.py"
)
assert _spec and _spec.loader
_ps = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_ps)
_ps.CFG_TEST_ATTR_RE = re.compile(r"#\[cfg\((?:test|all\(test\b[^\]]*\))\)\]")

TOKEN_RE = re.compile(
    r'"(?:self|\?CLASS|\?ROLE)"'
    r"|\bwk::(?:self_|class_decl|role_decl)\(\)"
)


def strip_line_comment(line: str) -> str:
    """Cut a `//` comment, ignoring a `//` inside a string literal."""
    in_str = False
    i = 0
    while i < len(line):
        c = line[i]
        if in_str:
            if c == "\\":
                i += 1
            elif c == '"':
                in_str = False
        elif c == '"':
            in_str = True
        elif c == "/" and line.startswith("//", i):
            return line[:i]
        i += 1
    return line


def count_source(src: str) -> int:
    masked = _ps.mask_non_code(src)
    blanked = _ps.blank_test_regions(masked)
    total = 0
    for line, m_line, b_line in zip(
        src.splitlines(), masked.splitlines(), blanked.splitlines()
    ):
        # A line of code inside a `#[cfg(test)]` region is blanked.
        if m_line.strip() and not b_line.strip():
            continue
        total += len(TOKEN_RE.findall(strip_line_comment(line)))
    return total


def scanned_files() -> list[Path]:
    files: list[Path] = [SRC / name for name in ROOT_FILES if (SRC / name).exists()]
    for layer in LAYERS:
        base = SRC / layer
        if base.is_dir():
            files.extend(sorted(base.rglob("*.rs")))
    return files


def current_counts() -> dict[str, int]:
    skip = _ps.test_only_module_files()
    counts: dict[str, int] = {}
    for path in scanned_files():
        if path.resolve() in skip:
            continue
        n = count_source(path.read_text(encoding="utf-8"))
        if n:
            counts[path.relative_to(ROOT).as_posix()] = n
    return counts


def read_baseline() -> dict[str, int]:
    counts: dict[str, int] = {}
    for line in BASELINE.read_text(encoding="utf-8").splitlines():
        body = line.split("#", 1)[0].strip()
        if not body:
            continue
        path, n = body.split()[:2]
        counts[path] = int(n)
    return counts


HEADER = """\
# References to the fixed frame names (`self`, `?CLASS`, `?ROLE`) in the runtime
# layers, per file (see scripts/check-frame-fixtures.py, ADR-14843). Format:
# <path> <count>. Counts may only go down. Re-cut with
#   scripts/check-frame-fixtures.py --update
# after removing a reference. A new reference or file means editing this list by
# hand, which a reviewer sees: say why the name is not read from the frame
# record (`frame_invocant`, `frame_fixture_value`).
"""


def write_baseline(counts: dict[str, int]) -> None:
    width = max((len(p) for p in counts), default=0)
    lines = [HEADER]
    for path in sorted(counts):
        lines.append(f"{path.ljust(width)} {counts[path]:>3}")
    BASELINE.write_text("\n".join(lines) + "\n", encoding="utf-8")


def check() -> int:
    base = read_baseline()
    now = current_counts()
    bad: list[str] = []
    for path, n in sorted(now.items()):
        allowed = base.get(path, 0)
        if n > allowed:
            bad.append(f"  {path}: {n} references, baseline allows {allowed}")
    if bad:
        print("check-frame-fixtures: new references to the fixed frame names:")
        print("\n".join(bad))
        print(
            "Read `self`/`?CLASS`/`?ROLE` from the frame record (frame_invocant,\n"
            "frame_fixture_value) and store through store_invocant_by_name; an Env\n"
            "key for them is what ADR-14843 retired."
        )
        return 1
    stale = [p for p, n in sorted(now.items()) if n < base.get(p, 0)]
    stale += [p for p in sorted(base) if p not in now]
    if stale:
        print("check-frame-fixtures: the baseline is above the current counts;")
        print("re-cut it with scripts/check-frame-fixtures.py --update:")
        print("\n".join(f"  {p}" for p in stale))
        return 1
    print(f"check-frame-fixtures: ok ({sum(now.values())} references in {len(now)} files)")
    return 0


def self_test() -> int:
    cases = [
        ('fn f() { env.get("self"); }', 1),
        ("fn f() { x == wk::self_() || y == wk::class_decl() }", 2),
        ('// "self" in a comment\nfn f() {}', 0),
        ('fn f() { let a = 1; } // "?ROLE"', 0),
        ('#[cfg(test)]\nmod t {\n    fn f() { env.get("self"); }\n}\n', 0),
        ('fn f() { g("a//b", "?CLASS"); }', 1),
        ('fn f() { g("selfish"); }', 0),
    ]
    failed = 0
    for src, want in cases:
        got = count_source(src)
        if got != want:
            print(f"self-test FAILED: {src!r}: got {got}, want {want}")
            failed += 1
    if failed:
        return 1
    print("check-frame-fixtures: self-test ok")
    return 0


def main(argv: list[str]) -> int:
    if "--self-test" in argv:
        return self_test()
    if "--update" in argv:
        write_baseline(current_counts())
        print(f"wrote {BASELINE.relative_to(ROOT)}")
        return 0
    return check()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
