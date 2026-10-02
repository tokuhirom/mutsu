#!/usr/bin/env python3
"""Ratchet on hand-rolled AST walkers (ADR-0137).

An analysis that asks a question about an AST subtree implements
`crate::ast_visit::Visit` and lets the exhaustive `walk_*` functions do the
recursion. A private recursive `match` over `Stmt`/`Expr` duplicates that
recursion, and its `_ =>` arm silently skips every variant it forgot -- or
that is added later. About a hundred such walkers predate the visitor; they
are listed with their per-file counts in scripts/ast-walkers-baseline.txt and
are ported over time. A new walker anywhere, or a higher count in a listed
file, fails. A count that falls must be re-cut, so the list only ever shrinks:

    scripts/check-ast-walkers.py              # check
    scripts/check-ast-walkers.py --update     # re-cut after porting a walker
    scripts/check-ast-walkers.py --self-test

A "walker" here is a function that takes a `Stmt` or `Expr` (in any form:
`&Stmt`, `&[Stmt]`, `&mut Expr`, `Vec<Stmt>`, ...), matches on `Stmt::` /
`Expr::` variants in its body, and is recursive -- it calls itself, or is part
of a cycle of such functions in the same file. Code generation (the compiler's
`compile_*` cluster, TRIR, RakuAST conversion) and recursion that follows one
path (an lvalue chain, a statement tail) match that shape too; they are listed
in the baseline with the rest. A structural walk that must NOT visit every
child (the sink-context propagation in `parser/sink_warn.rs`, a placeholder
analysis that stops at block boundaries) may stay hand-rolled: override the
visitor's hooks first, and keep it hand-rolled only when that cannot express
it.

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
BASELINE = ROOT / "scripts" / "ast-walkers-baseline.txt"
VISITOR_DIR = SRC / "ast_visit"

_spec = importlib.util.spec_from_file_location(
    "panic_surface", ROOT / "scripts" / "check-panic-surface.py"
)
assert _spec and _spec.loader
_ps = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_ps)
_ps.CFG_TEST_ATTR_RE = re.compile(r"#\[cfg\((?:test|all\(test\b[^\]]*\))\)\]")

FN_RE = re.compile(r"\bfn\s+([A-Za-z_][A-Za-z0-9_]*)\s*(?:<[^{;]*?>)?\s*\(")
AST_TYPE_RE = re.compile(r"\b(?:Stmt|Expr)\b")
VARIANT_RE = re.compile(r"\b(?:Stmt|Expr)::[A-Z]")


def _matching(src: str, open_idx: int, open_ch: str, close_ch: str) -> int:
    depth = 0
    for i in range(open_idx, len(src)):
        c = src[i]
        if c == open_ch:
            depth += 1
        elif c == close_ch:
            depth -= 1
            if depth == 0:
                return i
    return len(src) - 1


def functions(src: str) -> list[tuple[str, str, str]]:
    """(name, parameter list, body) of every `fn` with a body, nested ones too."""
    out = []
    for m in FN_RE.finditer(src):
        params_open = m.end() - 1
        params_close = _matching(src, params_open, "(", ")")
        brace = src.find("{", params_close)
        semi = src.find(";", params_close)
        if brace == -1 or (semi != -1 and semi < brace):
            continue  # a trait method signature without a body
        body_close = _matching(src, brace, "{", "}")
        out.append((m.group(1), src[params_open : params_close + 1], src[brace : body_close + 1]))
    return out


def walker_count(src: str) -> int:
    fns = [
        (name, body)
        for name, params, body in functions(src)
        if AST_TYPE_RE.search(params) and VARIANT_RE.search(body)
    ]
    names = {name for name, _ in fns}
    calls: dict[str, set[str]] = {}
    for name, body in fns:
        # Any reference counts: a call, a method call, or the function passed
        # by name (`.any(walk)`).
        called = {n for n in names if re.search(rf"(?<![A-Za-z0-9_]){re.escape(n)}(?![A-Za-z0-9_])", body)}
        calls.setdefault(name, set()).update(called)

    def reaches(start: str, goal: str) -> bool:
        seen, stack = set(), [start]
        while stack:
            cur = stack.pop()
            for nxt in calls.get(cur, ()):
                if nxt == goal:
                    return True
                if nxt not in seen:
                    seen.add(nxt)
                    stack.append(nxt)
        return False

    return sum(1 for name, _ in fns if reaches(name, name))


def count_masked(src: str) -> int:
    return walker_count(_ps.blank_test_regions(_ps.mask_non_code(src)))


def current_counts() -> dict[str, int]:
    skip = _ps.test_only_module_files()
    counts: dict[str, int] = {}
    for path in sorted(SRC.rglob("*.rs")):
        if path.resolve() in skip or VISITOR_DIR in path.parents:
            continue
        n = count_masked(path.read_text(encoding="utf-8"))
        if n:
            counts[path.relative_to(ROOT).as_posix()] = n
    return counts


def read_baseline() -> tuple[dict[str, int], dict[str, str]]:
    counts: dict[str, int] = {}
    notes: dict[str, str] = {}
    for line in BASELINE.read_text(encoding="utf-8").splitlines():
        body = line.split("#", 1)[0].strip()
        if not body:
            continue
        path, n = body.split()[:2]
        counts[path] = int(n)
        if "#" in line:
            notes[path] = line.split("#", 1)[1].strip()
    return counts, notes


HEADER = """\
# Hand-rolled recursive Stmt/Expr walkers, per file (see
# scripts/check-ast-walkers.py, ADR-0137). Format: <path> <count>  # <note>
# Counts may only go down: port a walker onto crate::ast_visit::Visit and
# re-cut with
#   scripts/check-ast-walkers.py --update
# Adding a walker means editing this list by hand, which a reviewer sees: say
# why the visitor cannot express it.
"""


def write_baseline(counts: dict[str, int], notes: dict[str, str]) -> None:
    width = max((len(p) for p in counts), default=0)
    lines = [HEADER]
    for path in sorted(counts):
        note = notes.get(path, "")
        row = f"{path.ljust(width)} {counts[path]:>2}"
        lines.append(f"{row}  # {note}" if note else row)
    BASELINE.write_text("\n".join(lines) + "\n", encoding="utf-8")


def self_test() -> int:
    snippet = """
fn direct(s: &Stmt) -> bool {
    match s { Stmt::Block(b) => b.iter().any(direct), _ => false }
}
fn even(e: &Expr) -> bool { match e { Expr::Grouped(i) => odd(i), _ => true } }
fn odd(e: &Expr) -> bool { match e { Expr::Grouped(i) => even(i), _ => false } }
fn shallow(s: &Stmt) -> bool { matches!(s, Stmt::Block(_)) }
fn recursive_but_not_ast(n: u32) -> u32 { if n == 0 { 0 } else { recursive_but_not_ast(n - 1) } }
// fn commented(s: &Stmt) { match s { Stmt::Block(b) => commented(s) } }
fn visitor(v: &mut V, s: &Stmt) { walk_stmt(v, s) }
#[cfg(test)]
mod tests {
    fn in_test(s: &Stmt) { match s { Stmt::Block(_) => in_test(s), _ => {} } }
}
"""
    got = count_masked(snippet)
    if got != 3:
        print(f"check-ast-walkers: self-test expected 3, got {got}", file=sys.stderr)
        return 1
    print("check-ast-walkers: self-test ok")
    return 0


def main(argv: list[str]) -> int:
    if "--self-test" in argv:
        return self_test()
    current = current_counts()
    if "--init" in argv:
        write_baseline(current, {})
        print(f"ast-walkers baseline written: {sum(current.values())} walkers in {len(current)} files")
        return 0
    allowed, notes = read_baseline()
    if "--update" in argv:
        grown = {p: n for p, n in current.items() if n > allowed.get(p, 0)}
        if grown:
            for p, n in sorted(grown.items()):
                print(f"check-ast-walkers: {p} has {n} (allowed {allowed.get(p, 0)});"
                      " --update only shrinks the list -- add it by hand", file=sys.stderr)
            return 1
        write_baseline(current, notes)
        print(f"ast-walkers baseline re-cut: {sum(current.values())} walkers in {len(current)} files")
        return 0

    failed = False
    for path in sorted(set(current) | set(allowed)):
        n, a = current.get(path, 0), allowed.get(path, 0)
        if n > a:
            failed = True
            print(f"check-ast-walkers: {path}: {n} hand-rolled AST walker(s), allowed {a}",
                  file=sys.stderr)
        elif n < a:
            failed = True
            print(f"check-ast-walkers: {path}: fell from {a} to {n} -- re-cut:\n"
                  "  scripts/check-ast-walkers.py --update", file=sys.stderr)
    if failed:
        print(__doc__.split("\n\n", 2)[1], file=sys.stderr)
        return 1
    print(f"check-ast-walkers: {sum(current.values())} walkers in {len(current)} files "
          "(all in the baseline)")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
