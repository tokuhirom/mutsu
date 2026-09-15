#!/usr/bin/env python3
"""Ratchet on the panic-family / `#[allow(` surface in src/ (issue #8186).

PLAN.md states the goal that mutsu must never Rust-panic on any input, but
nothing enforced it: the count of `unwrap`/`expect`/`panic!`/`unreachable!`/
`todo!`/`unimplemented!` in src/ rose at every architecture review taken so
far (2,440 as of 2026-09-12), and `#[allow(` rose alongside it (243). This
script is the "someone chose" ratchet from #8186's option 1: it counts both
surfaces and fails when either count exceeds the committed baseline in
scripts/panic-surface-baseline.txt.

The count is a blunt grep-family metric on purpose (stable, hard to game),
NOT a quality signal by itself -- plenty of the counted sites are provably
unreachable. What it guarantees is that the total can only go down or stay
flat; raising it requires deliberately editing the baseline, which makes
growth visible in review instead of invisible in aggregate.

Test scaffolding is excluded: `#[cfg(test)]` items (a `mod tests { ... }`
block, or an individual `#[cfg(test)] fn ...`) are blanked out before
counting, so unit tests do not consume the production budget. This needs a
real (if approximate) Rust-comment/string/brace scan rather than a line-based
grep, because `#[cfg(test)]` blocks nest at arbitrary depth and are not
always the last item in a file.

A test module split into its own file -- `#[cfg(test)] #[path = "x_tests.rs"]
mod tests;`, which is how a module keeps its unit tests while staying inside
the repository's 500-line limit -- is excluded as a whole file. Nothing in it
is reachable outside `cfg(test)`, so counting it would make following that
convention cost production budget.

    scripts/check-panic-surface.py              # check against the baseline
    scripts/check-panic-surface.py --update     # re-baseline after a change

`make check-panic-surface` runs the check; it is a `make test` prerequisite.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SRC = ROOT / "src"
BASELINE = ROOT / "scripts" / "panic-surface-baseline.txt"

PANIC_RE = re.compile(
    r"\.unwrap\(\)|\.unwrap_err\(\)|\.expect\(|\bpanic!\(|\bunreachable!\("
    r"|\btodo!\(|\bunimplemented!\("
)
ALLOW_RE = re.compile(r"#\[allow\(")

CFG_TEST_ATTR_RE = re.compile(r"#\[cfg\(test\)\]")
# `#[cfg(test)] #[path = "foo_tests.rs"] mod tests;`, in either attribute
# order and with other attributes stacked in between. Matched against the RAW
# source (not the masked form) because the path is a string literal.
CFG_TEST_PATH_MOD_RE = re.compile(
    r"#\[cfg\(test\)\]\s*(?:#\[[^\]]*\]\s*)*?#\[path\s*=\s*\"(?P<path>[^\"]+)\"\]"
    r"\s*(?:#\[[^\]]*\]\s*)*mod\s+\w+\s*;"
    r"|#\[path\s*=\s*\"(?P<path2>[^\"]+)\"\]\s*(?:#\[[^\]]*\]\s*)*?#\[cfg\(test\)\]"
    r"\s*(?:#\[[^\]]*\]\s*)*mod\s+\w+\s*;"
)
CHAR_LIT_RE = re.compile(r"'(\\u\{[0-9a-fA-F]+\}|\\.|[^'\\])'")


def mask_non_code(src: str) -> str:
    """Blank comments and string/char literals, preserving length and
    newlines, so brace and pattern counting only sees real code tokens."""
    out = list(src)
    n = len(src)
    i = 0
    while i < n:
        c = src[i]
        if c == "/" and i + 1 < n and src[i + 1] == "/":
            j = i
            while j < n and src[j] != "\n":
                out[j] = " "
                j += 1
            i = j
        elif c == "/" and i + 1 < n and src[i + 1] == "*":
            depth = 1
            out[i] = out[i + 1] = " "
            j = i + 2
            while j < n and depth > 0:
                if src[j : j + 2] == "/*":
                    out[j] = out[j + 1] = " "
                    depth += 1
                    j += 2
                elif src[j : j + 2] == "*/":
                    out[j] = out[j + 1] = " "
                    depth -= 1
                    j += 2
                else:
                    if src[j] != "\n":
                        out[j] = " "
                    j += 1
            i = j
        elif c == '"':
            j = i + 1
            out[i] = " "
            while j < n and src[j] != '"':
                if src[j] == "\\" and j + 1 < n:
                    if src[j] != "\n":
                        out[j] = " "
                    j += 1
                if j < n and src[j] != "\n":
                    out[j] = " "
                j += 1
            if j < n:
                out[j] = " "
                j += 1
            i = j
        elif c == "r" and i + 1 < n and src[i + 1] in ('"', "#"):
            j = i + 1
            hashes = 0
            while j < n and src[j] == "#":
                hashes += 1
                j += 1
            if j < n and src[j] == '"':
                start = i
                j += 1
                closer = '"' + "#" * hashes
                idx = src.find(closer, j)
                end = n if idx == -1 else idx + len(closer)
                for k in range(start, end):
                    if src[k] != "\n":
                        out[k] = " "
                i = end
            else:
                i += 1
        elif c == "'":
            m = CHAR_LIT_RE.match(src[i : i + 12])
            if m:
                length = m.end()
                for k in range(length):
                    if src[i + k] != "\n":
                        out[i + k] = " "
                i += length
            else:
                i += 1
        else:
            i += 1
    return "".join(out)


def _skip_attributes(masked: str, pos: int) -> int:
    """Advance past whitespace and any further `#[...]` attributes stacked
    on top of the one already matched, returning the position of the item
    they annotate."""
    n = len(masked)
    while True:
        while pos < n and masked[pos] in " \t\r\n":
            pos += 1
        if pos < n and masked[pos] == "#" and pos + 1 < n and masked[pos + 1] == "[":
            depth = 0
            j = pos + 1
            while j < n:
                if masked[j] == "[":
                    depth += 1
                elif masked[j] == "]":
                    depth -= 1
                    if depth == 0:
                        j += 1
                        break
                j += 1
            pos = j
            continue
        return pos


def blank_test_regions(masked: str) -> str:
    """Blank the body of every `#[cfg(test)]`-annotated item: either the
    balanced `{ ... }` block of a fn/mod/impl, or a single `...;` statement
    when the item has no block (e.g. a bare `use` behind cfg(test))."""
    out = list(masked)
    n = len(masked)
    for m in CFG_TEST_ATTR_RE.finditer(masked):
        pos = _skip_attributes(masked, m.end())
        depth_paren = depth_brack = 0
        j = pos
        found_brace = found_semi = None
        while j < n:
            ch = masked[j]
            if ch == "(":
                depth_paren += 1
            elif ch == ")":
                depth_paren -= 1
            elif ch == "[":
                depth_brack += 1
            elif ch == "]":
                depth_brack -= 1
            elif ch == "{" and depth_paren == 0 and depth_brack == 0:
                found_brace = j
                break
            elif ch == ";" and depth_paren == 0 and depth_brack == 0:
                found_semi = j
                break
            j += 1
        if found_brace is not None:
            depth = 0
            k = found_brace
            while k < n:
                if masked[k] == "{":
                    depth += 1
                elif masked[k] == "}":
                    depth -= 1
                    if depth == 0:
                        k += 1
                        break
                k += 1
            end = k
        elif found_semi is not None:
            end = found_semi + 1
        else:
            end = n
        # Blank from the `#[cfg(test)]` attribute itself (not just its body),
        # so a stacked `#[allow(...)]`/`#[test]` on the same test-only item
        # does not consume the production `#[allow(` budget either.
        for idx in range(m.start(), end):
            if out[idx] != "\n":
                out[idx] = " "
    return "".join(out)


def counts_for_file(path: Path) -> tuple[int, int]:
    src = path.read_text(encoding="utf-8")
    masked = blank_test_regions(mask_non_code(src))
    return len(PANIC_RE.findall(masked)), len(ALLOW_RE.findall(masked))


def test_only_module_files() -> set[Path]:
    """Files declared as a whole module behind `#[cfg(test)]` via `#[path]`.

    Such a file is unit-test scaffolding in its entirety -- it has no other
    declaration site, so nothing in it compiles outside `cfg(test)` -- and is
    skipped rather than blanked, since there is no `#[cfg(test)]` attribute
    inside it for `blank_test_regions` to key on.
    """
    found: set[Path] = set()
    for path in sorted(SRC.rglob("*.rs")):
        src = path.read_text(encoding="utf-8")
        for m in CFG_TEST_PATH_MOD_RE.finditer(src):
            rel = m.group("path") or m.group("path2")
            target = (path.parent / rel).resolve()
            if target.is_file():
                found.add(target)
    return found


def total_counts() -> tuple[int, int]:
    panics = allows = 0
    skip = test_only_module_files()
    for path in sorted(SRC.rglob("*.rs")):
        if path.resolve() in skip:
            continue
        p, a = counts_for_file(path)
        panics += p
        allows += a
    return panics, allows


def read_baseline() -> tuple[int, int]:
    values: dict[str, int] = {}
    for line in BASELINE.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        key, _, value = line.partition("=")
        values[key.strip()] = int(value.strip())
    return values["panic_surface"], values["allow_surface"]


def write_baseline(panic_surface: int, allow_surface: int) -> None:
    BASELINE.write_text(
        "# Ratchet baseline for scripts/check-panic-surface.py (issue #8186).\n"
        "# These counts may go down (or stay flat), never up. Regenerate with:\n"
        "#   scripts/check-panic-surface.py --update\n"
        f"panic_surface={panic_surface}\n"
        f"allow_surface={allow_surface}\n",
        encoding="utf-8",
    )


def _count_snippet(src: str) -> tuple[int, int]:
    masked = blank_test_regions(mask_non_code(src))
    return len(PANIC_RE.findall(masked)), len(ALLOW_RE.findall(masked))


def self_test() -> int:
    """Exercise the masking logic against synthetic snippets so a change to
    the tokenizer can't silently make the ratchet fail open (report 0 no
    matter what src/ actually contains)."""
    cases: list[tuple[str, str, tuple[int, int]]] = [
        ("plain unwrap", "fn f() { x.unwrap(); }", (1, 0)),
        ("plain panic!", 'fn f() { panic!("boom"); }', (1, 0)),
        ("plain expect", 'fn f() { x.expect("y"); }', (1, 0)),
        ("plain unreachable!", "fn f() { unreachable!(); }", (1, 0)),
        ("plain todo!/unimplemented!", "fn f() { todo!(); unimplemented!(); }", (2, 0)),
        ("plain allow", "#[allow(dead_code)]\nfn f() {}", (0, 1)),
        ("line comment", "// x.unwrap();\nfn f() {}", (0, 0)),
        ("block comment", "/* x.unwrap(); */\nfn f() {}", (0, 0)),
        ("nested block comment", "/* outer /* x.unwrap(); */ still */\nfn f() {}", (0, 0)),
        ("string literal", 'let s = "panic!(";', (0, 0)),
        ("string with escaped quote", 'let s = "a\\"panic!(\\"b";', (0, 0)),
        ("raw string", 'let s = r#"unreachable!("#;', (0, 0)),
        ("char literal not a string opener", "let c = '\"'; x.unwrap();", (1, 0)),
        (
            "cfg(test) mod block excluded",
            "fn a() { x.unwrap(); }\n"
            "#[cfg(test)]\n"
            "mod tests {\n"
            "    fn t() { y.unwrap(); }\n"
            "}\n"
            "fn b() { z.unwrap(); }\n",
            (2, 0),
        ),
        (
            "cfg(test) fn without block excluded",
            "#[cfg(test)]\nuse foo::bar;\nfn a() { x.unwrap(); }\n",
            (1, 0),
        ),
        (
            "stacked attributes before cfg(test) item",
            "#[cfg(test)]\n#[allow(dead_code)]\nfn t() { x.unwrap(); }\n"
            "fn b() { y.unwrap(); }\n",
            (1, 0),
        ),
        (
            "nested cfg(test) mod inside non-test mod",
            "mod outer {\n"
            "    fn a() { x.unwrap(); }\n"
            "    #[cfg(test)]\n"
            "    mod tests {\n"
            "        fn t() { y.unwrap(); }\n"
            "    }\n"
            "}\n",
            (1, 0),
        ),
    ]

    failures = 0
    for name, src, expected in cases:
        got = _count_snippet(src)
        if got != expected:
            print(
                f"check-panic-surface self-test FAILED: {name!r}: "
                f"expected panic={expected[0]},allow={expected[1]}, "
                f"got panic={got[0]},allow={got[1]}",
                file=sys.stderr,
            )
            failures += 1

    # The file-level rule, which the snippet cases above cannot reach: a
    # `#[path]` module is skipped only when it is ALSO behind `#[cfg(test)]`.
    # Getting this wrong in the permissive direction would let production code
    # hide from the ratchet behind a `#[path]`, so the negative case matters
    # as much as the positive ones.
    path_cases: list[tuple[str, str, str | None]] = [
        (
            "cfg(test) before path",
            '#[cfg(test)]\n#[path = "a_tests.rs"]\nmod tests;\n',
            "a_tests.rs",
        ),
        (
            "path before cfg(test)",
            '#[path = "b_tests.rs"]\n#[cfg(test)]\nmod tests;\n',
            "b_tests.rs",
        ),
        (
            "stacked attribute in between",
            '#[cfg(test)]\n#[allow(dead_code)]\n#[path = "c_tests.rs"]\nmod tests;\n',
            "c_tests.rs",
        ),
        (
            "path module without cfg(test) is production code",
            '#[path = "d.rs"]\nmod d;\n',
            None,
        ),
        (
            "cfg(test) module without path is blanked, not skipped",
            "#[cfg(test)]\nmod tests { fn f() { x.unwrap(); } }\n",
            None,
        ),
    ]
    for name, src, expected_path in path_cases:
        m = CFG_TEST_PATH_MOD_RE.search(src)
        got_path = (m.group("path") or m.group("path2")) if m else None
        if got_path != expected_path:
            print(
                f"check-panic-surface self-test FAILED: {name!r}: "
                f"expected path={expected_path!r}, got path={got_path!r}",
                file=sys.stderr,
            )
            failures += 1
    if failures:
        print(f"check-panic-surface self-test: {failures} failure(s)", file=sys.stderr)
        return 1
    print(
        f"check-panic-surface self-test: all "
        f"{len(cases) + len(path_cases)} cases pass"
    )
    return 0


def main(argv: list[str]) -> int:
    if "--self-test" in argv:
        return self_test()

    panic_now, allow_now = total_counts()

    if "--update" in argv:
        write_baseline(panic_now, allow_now)
        print(
            f"check-panic-surface: baseline updated "
            f"(panic_surface={panic_now}, allow_surface={allow_now})"
        )
        return 0

    if not BASELINE.exists():
        print(
            f"check-panic-surface: {BASELINE} missing; run "
            "scripts/check-panic-surface.py --update",
            file=sys.stderr,
        )
        return 1

    panic_base, allow_base = read_baseline()
    fail = False

    if panic_now > panic_base:
        print(
            f"check-panic-surface ratchet FAILED: {panic_now} panic-family sites "
            f"in src/ (unwrap/expect/panic!/unreachable!/todo!/unimplemented!), "
            f"baseline {panic_base}.",
            file=sys.stderr,
        )
        print(
            "New code must handle the error instead of panicking, or return a "
            "RuntimeError -- see PLAN.md's never-panic goal (#8186).",
            file=sys.stderr,
        )
        fail = True
    elif panic_now < panic_base:
        print(
            f"check-panic-surface: panic_surface={panic_now} (baseline "
            f"{panic_base}) -- lower the baseline: "
            "scripts/check-panic-surface.py --update"
        )

    if allow_now > allow_base:
        print(
            f"check-panic-surface ratchet FAILED: {allow_now} `#[allow(` sites "
            f"in src/, baseline {allow_base}.",
            file=sys.stderr,
        )
        print(
            "Fix the lint instead of suppressing it, or justify a narrower "
            "allow -- see #8186.",
            file=sys.stderr,
        )
        fail = True
    elif allow_now < allow_base:
        print(
            f"check-panic-surface: allow_surface={allow_now} (baseline "
            f"{allow_base}) -- lower the baseline: "
            "scripts/check-panic-surface.py --update"
        )

    if fail:
        return 1

    print(
        f"check-panic-surface: ok (panic_surface={panic_now}/{panic_base}, "
        f"allow_surface={allow_now}/{allow_base})"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
