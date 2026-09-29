#!/usr/bin/env python3
"""Ratchet on constructing an `Interpreter` outside the places that need one.

An `Interpreter` is a whole runtime: `Interpreter::new()` reads the process
environment into `%*ENV`, seeds the IO handle table, and installs the builtin
declaration registry. Building one to evaluate a closure or an expression is
the most expensive way there is to call code, and it also runs that code in
the wrong runtime: not the caller's env, registry, pragmas, or IO handles.
`builtins::methods_narg::buf::eval_whatever_code` did exactly that for every
`$buf.subbuf(*-2)` until #10118 removed it.

Legitimate construction sites are few:

  * process entry points (`main.rs`, `lib.rs`, the REPL, `--doc`);
  * spawning a thread (`clone_for_thread`), since each thread owns its
    interpreter;
  * the parse-time module probes (`slang_activation.rs`,
    `parse_time_exports.rs`), which run a module on a fresh thread once per
    `use`;
  * a `thread_local!` built once per thread (`regex_parse.rs`).

All of them are listed with their per-file counts in
scripts/interp-construction-allowlist.txt. A new site anywhere else, or a
higher count in a listed file, fails. A count that falls must be re-cut, so the list only ever shrinks:

    scripts/check-interp-construction.py              # check
    scripts/check-interp-construction.py --update     # re-cut after removing a site
    scripts/check-interp-construction.py --self-test

To call a closure from runtime code, run its compiled bytecode on the
interpreter you already have (`call_compiled_closure`, `vm_call_on_value`,
`call_subscript_code`). A pure builtin that cannot reach an interpreter must
decline the call, or the VM must resolve the Callable argument before calling
the builtin (see `resolve_subbuf_callable_args`).

Test code is excluded the same way scripts/check-panic-surface.py excludes it
(`#[cfg(test)]` items are blanked, whole `#[cfg(test)] #[path]` test files are
skipped), and so are comments and string literals.
"""
from __future__ import annotations

import importlib.util
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SRC = ROOT / "src"
ALLOWLIST = ROOT / "scripts" / "interp-construction-allowlist.txt"

_spec = importlib.util.spec_from_file_location(
    "panic_surface", ROOT / "scripts" / "check-panic-surface.py"
)
assert _spec and _spec.loader
_ps = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_ps)
# Also treat `#[cfg(all(test, ...))]` items as test code (vm_poll.rs's JIT
# tests); check-panic-surface.py only keys on the plain `#[cfg(test)]`.
_ps.CFG_TEST_ATTR_RE = re.compile(r"#\[cfg\((?:test|all\(test\b[^\]]*\))\)\]")

# Every spelling that yields a new `Interpreter`. The `fn` definitions are not
# matched: each pattern needs a `::` / `.` receiver in front of the name. A
# struct literal filling the rest from `..Default::default()` is
# `Interpreter::new()` too (comments are masked before matching, so the
# literal's body holds no braces).
CONSTRUCT_RE = re.compile(
    r"\bInterpreter::(?:new|default)\s*\("
    r"|\bInterpreter\s*\{[^{}]*\.\.\s*Default::default\s*\("
    r"|(?<!\.tap)\.clone_for_thread\s*\("
)


def count_masked(src: str) -> int:
    return len(CONSTRUCT_RE.findall(_ps.blank_test_regions(_ps.mask_non_code(src))))


def current_counts() -> dict[str, int]:
    skip = _ps.test_only_module_files()
    counts: dict[str, int] = {}
    for path in sorted(SRC.rglob("*.rs")):
        if path.resolve() in skip:
            continue
        n = count_masked(path.read_text(encoding="utf-8"))
        if n:
            counts[path.relative_to(ROOT).as_posix()] = n
    return counts


def read_allowlist() -> tuple[dict[str, int], dict[str, str]]:
    counts: dict[str, int] = {}
    notes: dict[str, str] = {}
    for line in ALLOWLIST.read_text(encoding="utf-8").splitlines():
        body = line.split("#", 1)[0].strip()
        if not body:
            continue
        path, n = body.split()[:2]
        counts[path] = int(n)
        if "#" in line:
            notes[path] = line.split("#", 1)[1].strip()
    return counts, notes


HEADER = """\
# Allowed `Interpreter` construction sites, per file (see
# scripts/check-interp-construction.py). Format: <path> <count>  # <why>
# Counts may only go down. Re-cut with
#   scripts/check-interp-construction.py --update
# after removing a site. Adding a site or a file means editing this list by
# hand, which a reviewer sees: say why the new site cannot run on the caller's
# interpreter.
"""


def write_allowlist(counts: dict[str, int], notes: dict[str, str]) -> None:
    width = max((len(p) for p in counts), default=0)
    lines = [HEADER]
    for path in sorted(counts):
        note = notes.get(path, "")
        row = f"{path.ljust(width)} {counts[path]:>2}"
        lines.append(f"{row}  # {note}" if note else row)
    ALLOWLIST.write_text("\n".join(lines) + "\n", encoding="utf-8")


def self_test() -> int:
    snippet = """
fn a() { let i = Interpreter::new(); }
fn b() { let i = crate::runtime::Interpreter::default(); }
fn c(&self) { let s = Interpreter { env: self.env.clone(), ..Default::default() }; }
fn d(&self) { let s = Interpreter {
    env: self.env.clone(),
    current_package: Arc::new(RwLock::new(String::new())),
    ..Default::default()
}; }
fn d2() { let o = Other { ..Default::default() }; }
fn e(&mut self) { let t = self.clone_for_thread(); }
// Interpreter::new() in a comment does not count
fn f() { let s = "Interpreter::new()"; }
fn g(&mut self) { let t = self.tap.clone_for_thread(); }
pub(crate) fn clone_for_thread(&mut self) -> Self { todo!() }
#[cfg(test)]
mod tests {
    fn h() { let i = Interpreter::new(); }
}
#[cfg(all(test, feature = "jit"))]
mod jit_tests {
    fn k() { let i = Interpreter::new(); }
}
"""
    got = count_masked(snippet)
    if got != 5:
        print(f"check-interp-construction: self-test expected 5, got {got}", file=sys.stderr)
        return 1
    print("check-interp-construction: self-test ok")
    return 0


def main(argv: list[str]) -> int:
    if "--self-test" in argv:
        return self_test()
    current = current_counts()
    allowed, notes = read_allowlist()
    if "--update" in argv:
        grown = {p: n for p, n in current.items() if n > allowed.get(p, 0)}
        if grown:
            for p, n in sorted(grown.items()):
                print(f"check-interp-construction: {p} has {n} (allowed {allowed.get(p, 0)});"
                      " --update only shrinks the list -- add the site by hand", file=sys.stderr)
            return 1
        write_allowlist(current, notes)
        print(f"interp-construction allowlist re-cut: {sum(current.values())} sites in {len(current)} files")
        return 0

    failed = False
    for path in sorted(set(current) | set(allowed)):
        n, a = current.get(path, 0), allowed.get(path, 0)
        if n > a:
            failed = True
            print(f"check-interp-construction: {path}: {n} Interpreter construction site(s), "
                  f"allowed {a}", file=sys.stderr)
        elif n < a:
            failed = True
            print(f"check-interp-construction: {path}: fell from {a} to {n} -- re-cut:\n"
                  "  scripts/check-interp-construction.py --update", file=sys.stderr)
    if failed:
        print(__doc__.split("\n\n", 2)[1], file=sys.stderr)
        print("\n  See scripts/check-interp-construction.py for where to call code instead.",
              file=sys.stderr)
        return 1
    print(f"check-interp-construction: {sum(current.values())} sites in {len(current)} files "
          "(all allowlisted)")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
