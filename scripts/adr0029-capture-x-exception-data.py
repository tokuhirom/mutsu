#!/usr/bin/env python3
"""ADR-0029 Slice 2: mechanically capture the real `X::` exception shape.

Per ADR-0029 (docs/adr/0029-exception-class-role-membership.md), mutsu's
`register_x` hand-modelled `X::` ancestry as single inheritance for years,
which is wrong for the ~59% of `X::` classes that actually compose one or
more marker roles in rakudo (X::Comp, X::Syntax, X::IO, ...). This script is
the "never hand transcription" data source the ADR requires: it derives a
name list mechanically (never from an `X::`-prefix enumeration), asks a
*single* real `raku` process for each name's true `.^mro` / `.^roles`, and
diffs that against what mutsu currently reports (also via a single mutsu
process, using the same probe program).

Regeneration recipe:
    python3 scripts/adr0029-capture-x-exception-data.py
    # writes:
    #   TODO_roast/x-exception-role-membership.tsv       (Slice 3's input)
    #   TODO_roast/x-exception-role-membership-diff.tsv  (mutsu vs raku, today)
    # and prints a summary to stdout.

Requires `raku` on PATH and a built `target/debug/mutsu` (or set MUTSU_BIN).
"""

from __future__ import annotations

import re
import subprocess
import sys
from collections import Counter
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
PROBE = REPO_ROOT / "scripts" / "probe-x-exception-shape.raku"
OUT_DATA = REPO_ROOT / "TODO_roast" / "x-exception-role-membership.tsv"
OUT_DIFF = REPO_ROOT / "TODO_roast" / "x-exception-role-membership-diff.tsv"

# Hard rule (ADR-0029 Slice 2): the name list comes from what mutsu raises or
# is tested against -- never an `X::`-prefix enumeration. Two sources:
#   1. `throws-like` / `fails-like` / `isa-ok` expected types in t/ + roast/.
#   2. `X::...` string literals in mutsu's own source (what it can raise).
# Names that don't resolve to a real rakudo Exception subtype (test-local
# classes like `X::Boom`, or bare-package namespaces like `X::Numeric`) are
# dropped by the raku probe itself, not guessed at here.
TEST_ASSERTION_RE = re.compile(
    r"(?:throws-like|fails-like)\s*\{?[^,\n]*,\s*(X::[A-Za-z0-9_:]+)"
    r"|isa-ok\s+\S+,\s*(X::[A-Za-z0-9_:]+)"
)
SRC_LITERAL_RE = re.compile(r'"(X::[A-Za-z0-9_:]+)"')


def derive_name_list() -> list[str]:
    names: set[str] = set()
    for base in (REPO_ROOT / "t", REPO_ROOT / "roast"):
        for path in base.rglob("*.t"):
            text = path.read_text(errors="replace")
            for m in TEST_ASSERTION_RE.finditer(text):
                names.add(m.group(1) or m.group(2))
    for path in (REPO_ROOT / "src").rglob("*.rs"):
        text = path.read_text(errors="replace")
        for m in SRC_LITERAL_RE.finditer(text):
            names.add(m.group(1))
    return sorted(names)


def run_probe(interpreter: list[str], names: list[str]) -> dict[str, tuple[str, str, str] | None]:
    """Run the probe script once, feeding all names over stdin. Returns
    name -> None (not a real Exception subtype) or (mro, roles_direct,
    roles_all), each a comma-joined `.^name` list."""
    proc = subprocess.run(
        [*interpreter, str(PROBE)],
        input="\n".join(names) + "\n",
        capture_output=True,
        text=True,
        timeout=300,
    )
    if proc.returncode != 0:
        print(f"probe failed ({' '.join(interpreter)}): {proc.stderr}", file=sys.stderr)
        sys.exit(1)
    result: dict[str, tuple[str, str, str] | None] = {}
    for line in proc.stdout.splitlines():
        cols = line.split("\t")
        if len(cols) == 2 and cols[1] == "0":
            result[cols[0]] = None
        elif len(cols) == 5:
            result[cols[0]] = (cols[2], cols[3], cols[4])
    return result


def main() -> None:
    mutsu_bin = REPO_ROOT / "target" / "debug" / "mutsu"
    names = derive_name_list()
    print(f"derived {len(names)} candidate names (never hand-typed)")

    raku_data = run_probe(["raku"], names)
    real = {n: shape for n, shape in raku_data.items() if shape is not None}
    print(f"{len(real)} / {len(names)} are real rakudo Exception subtypes")

    OUT_DATA.parent.mkdir(parents=True, exist_ok=True)
    with OUT_DATA.open("w") as f:
        f.write("name\tmro\troles_direct\troles_all\n")
        for name in sorted(real):
            mro, roles_direct, roles_all = real[name]
            f.write(f"{name}\t{mro}\t{roles_direct}\t{roles_all}\n")
    print(f"wrote {OUT_DATA.relative_to(REPO_ROOT)}")

    mutsu_data = run_probe([str(mutsu_bin)], sorted(real))
    counts: Counter[str] = Counter()
    with OUT_DIFF.open("w") as f:
        f.write("name\tcategory\traku_mro\tmutsu_mro\traku_roles_all\tmutsu_roles_all\n")
        for name in sorted(real):
            raku_mro, _raku_roles_direct, raku_roles_all = real[name]
            mutsu_shape = mutsu_data.get(name)
            if mutsu_shape is None:
                category = "missing"
                mutsu_mro = mutsu_roles_all = ""
            else:
                mutsu_mro, _mutsu_roles_direct, mutsu_roles_all = mutsu_shape
                if mutsu_mro != raku_mro:
                    category = "wrong_mro"
                elif mutsu_roles_all != raku_roles_all:
                    category = "wrong_roles"
                else:
                    category = "match"
            counts[category] += 1
            if category != "match":
                f.write(
                    f"{name}\t{category}\t{raku_mro}\t{mutsu_mro}\t"
                    f"{raku_roles_all}\t{mutsu_roles_all}\n"
                )
    print(f"wrote {OUT_DIFF.relative_to(REPO_ROOT)}")
    for category in ("match", "wrong_mro", "wrong_roles", "missing"):
        print(f"  {category}: {counts[category]}")


if __name__ == "__main__":
    main()
