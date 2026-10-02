#!/usr/bin/env python3
"""Ratchet on upward references from the lower layers (issue #10779).

The lower layers -- the AST, the parser, `Value`, the bytecode (`opcode`),
`Env` and the GC -- should not name the layers built on top of them: the
runtime (`Interpreter`), the VM, the compiler and the builtins. Each such edge
is a module-dependency cycle, and the cycles are what keep the crate from
being split. Most of them are pure helpers or tables that live in the wrong
module and can simply move down; the essential ones (Raku runs code at parse
time: BEGIN, slangs, EVAL) are to go through a narrow trait instead of naming
`Interpreter`.

Every `crate::<upper>` path in a lower-layer file counts once (a `use` of
several items through one path counts once). Per-file counts live in
scripts/layer-deps-baseline.txt and may go down, never up. A count that falls
must be re-cut, so the list only ever shrinks:

    scripts/check-layer-deps.py              # check
    scripts/check-layer-deps.py --update     # re-cut after removing an edge
    scripts/check-layer-deps.py --self-test

Test code is excluded the same way scripts/check-panic-surface.py excludes it,
and so are comments and string literals.
"""
from __future__ import annotations

import importlib.util
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SRC = ROOT / "src"
BASELINE = ROOT / "scripts" / "layer-deps-baseline.txt"

_spec = importlib.util.spec_from_file_location(
    "panic_surface", ROOT / "scripts" / "check-panic-surface.py"
)
assert _spec and _spec.loader
_ps = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_ps)

# Lower-layer roots (directories and single-file modules), relative to src/.
LOWER = [
    "ast", "ast.rs", "parser", "value", "opcode.rs", "env.rs", "gc",
    # Leaf modules below all of them: name and key construction.
    "symbol.rs", "qualified.rs", "type_id.rs", "meta_ns.rs", "str_scan.rs",
]
# Modules above every lower layer. `crate::Interpreter` is lib.rs's re-export.
UPPER = ["runtime", "vm", "compiler", "builtins", "trir", "interpreter", "Interpreter"]
# The parser sits above the AST and `Value`, so naming it from below is upward
# too; from inside the parser it is not.
UPPER_UNLESS_PARSER = ["parser"]


def upper_re(in_parser: bool) -> re.Pattern[str]:
    names = UPPER + ([] if in_parser else UPPER_UNLESS_PARSER)
    return re.compile(r"\bcrate::(?:" + "|".join(names) + r")\b")


def count_masked(src: str, in_parser: bool) -> int:
    masked = _ps.blank_test_regions(_ps.mask_non_code(src))
    return len(upper_re(in_parser).findall(masked))


def lower_files() -> list[Path]:
    out: list[Path] = []
    for root in LOWER:
        p = SRC / root
        if p.is_dir():
            out.extend(sorted(p.rglob("*.rs")))
        elif p.is_file():
            out.append(p)
    return out


def current_counts() -> dict[str, int]:
    skip = _ps.test_only_module_files()
    counts: dict[str, int] = {}
    for path in lower_files():
        if path.resolve() in skip:
            continue
        in_parser = (SRC / "parser") in path.parents
        n = count_masked(path.read_text(encoding="utf-8"), in_parser)
        if n:
            counts[path.relative_to(ROOT).as_posix()] = n
    return counts


def read_baseline() -> dict[str, int]:
    counts: dict[str, int] = {}
    for line in BASELINE.read_text(encoding="utf-8").splitlines():
        body = line.split("#", 1)[0].strip()
        if body:
            path, n = body.split()[:2]
            counts[path] = int(n)
    return counts


HEADER = """\
# Upward references (crate::runtime / vm / compiler / builtins / trir /
# Interpreter, and crate::parser from below the parser) from the lower layers
# (ast, parser, value, opcode, env, gc), per file. See
# scripts/check-layer-deps.py and issue #10779. Format: <path> <count>
# Counts may only go down: move the helper down, or route the call through a
# trait, and re-cut with
#   scripts/check-layer-deps.py --update
"""


def write_baseline(counts: dict[str, int]) -> None:
    width = max((len(p) for p in counts), default=0)
    lines = [HEADER]
    for path in sorted(counts):
        lines.append(f"{path.ljust(width)} {counts[path]:>3}")
    BASELINE.write_text("\n".join(lines) + "\n", encoding="utf-8")


def self_test() -> int:
    snippet = """
use crate::runtime::{A, B};
use crate::value::Value;
fn f(i: &crate::Interpreter) { crate::builtins::x(); crate::parser::y(); }
// crate::vm::commented
fn s() -> &'static str { "crate::compiler::in_a_string" }
#[cfg(test)]
mod tests { use crate::runtime::T; }
"""
    below, inside = count_masked(snippet, False), count_masked(snippet, True)
    if (below, inside) != (4, 3):
        print(f"check-layer-deps: self-test expected (4, 3), got ({below}, {inside})",
              file=sys.stderr)
        return 1
    print("check-layer-deps: self-test ok")
    return 0


def main(argv: list[str]) -> int:
    if "--self-test" in argv:
        return self_test()
    current = current_counts()
    if "--init" in argv:
        write_baseline(current)
        print(f"layer-deps baseline written: {sum(current.values())} references in {len(current)} files")
        return 0
    allowed = read_baseline()
    if "--update" in argv:
        grown = {p: n for p, n in current.items() if n > allowed.get(p, 0)}
        if grown:
            for p, n in sorted(grown.items()):
                print(f"check-layer-deps: {p} has {n} (allowed {allowed.get(p, 0)});"
                      " --update only shrinks the list", file=sys.stderr)
            return 1
        write_baseline(current)
        print(f"layer-deps baseline re-cut: {sum(current.values())} references in {len(current)} files")
        return 0

    failed = False
    for path in sorted(set(current) | set(allowed)):
        n, a = current.get(path, 0), allowed.get(path, 0)
        if n > a:
            failed = True
            print(f"check-layer-deps: {path}: {n} upward reference(s), allowed {a}",
                  file=sys.stderr)
        elif n < a:
            failed = True
            print(f"check-layer-deps: {path}: fell from {a} to {n} -- re-cut:\n"
                  "  scripts/check-layer-deps.py --update", file=sys.stderr)
    if failed:
        print(__doc__.split("\n\n", 2)[1], file=sys.stderr)
        return 1
    print(f"check-layer-deps: {sum(current.values())} upward references in {len(current)} files "
          "(all in the baseline)")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
