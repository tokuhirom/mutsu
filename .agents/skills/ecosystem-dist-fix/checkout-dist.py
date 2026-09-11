#!/usr/bin/env python3
"""checkout-dist.py — lay one zef distribution and its dependency closure out on
disk so an agent can iterate on it.

`scripts/ecosystem-sweep.py` extracts into a temporary directory and deletes it,
which is right for a measurement and useless for debugging: fixing a
distribution means running the same test file thirty times while rebuilding the
interpreter in between. This does the same resolution and extraction, keeps the
tree, and prints the exact commands to run each side.

    .agents/skills/ecosystem-dist-fix/checkout-dist.py String::Utils

Writes to tmp/ecosystem/<Dist--Name>/ (gitignored) and prints a shell snippet
defining DIST, LIBS, MUTSU and RAKU. Re-running reuses what is already there
unless --force is given.

Same index, same tarball cache and same flat `-I` closure as the sweep, so a
green run here means the same thing a green record does. It does NOT sandbox:
you are running an unaudited third-party test suite on your own machine, so read
what you are about to run first, exactly as the sweep's `--sandbox none` escape
hatch expects for a single distribution you already trust.
"""

from __future__ import annotations

import argparse
import json
import os
import shlex
import sys

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))))
sys.path.insert(0, os.path.join(REPO, "scripts"))
import ecosystem_common as eco  # noqa: E402

OUT_ROOT = os.path.join(REPO, "tmp", "ecosystem")


def slug(name: str) -> str:
    return name.replace("::", "--")


def place(name: str, index, dest: str, force: bool) -> str | None:
    """Extract `name` under `dest` and return its distribution root."""
    marker = os.path.join(dest, ".root")
    if os.path.exists(marker) and not force:
        with open(marker, encoding="utf-8") as fh:
            root = fh.read().strip()
        if os.path.isdir(root):
            return root
    url = index.url(name)
    if url is None:
        return None
    eco.rmtree(dest)
    root = eco.extract_dist(eco.fetch_tarball(url, os.path.join(eco.CACHE_DIR, "tarballs")),
                            dest)
    if root:
        with open(marker, "w", encoding="utf-8") as fh:
            fh.write(root)
    return root


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("dist", help="distribution name, e.g. String::Utils")
    ap.add_argument("--force", action="store_true", help="re-extract even if present")
    ap.add_argument("--refresh-index", action="store_true")
    ap.add_argument("--json", action="store_true", help="emit the plan as JSON")
    args = ap.parse_args()

    if os.environ.get("MUTSU_FUDGE"):
        return err("MUTSU_FUDGE is set; it is roast-only and would drop statements. Unset it.")

    index = eco.load_index(refresh=args.refresh_index)
    name = args.dist
    if name not in index.dists:
        target = index.resolve_name(name)
        if target is None:
            return err(f"{name} is in neither the fez nor the REA index. "
                       "Check the spelling, or pass the distribution name rather than a "
                       "module name.")
        print(f"note: {name} is a module provided by {target}; using that distribution",
              file=sys.stderr)
        name = target

    resolved, unresolved = index.closure(name)
    base = os.path.join(OUT_ROOT, slug(name))
    root = place(name, index, os.path.join(base, "dist"), args.force)
    if root is None:
        return err(f"{name} has no usable tarball / META6.json in the index")

    libs = [os.path.join(root, "lib")]
    dep_roots = {}
    for dep in resolved:
        dep_root = place(dep, index, os.path.join(base, "deps", slug(dep)), args.force)
        if dep_root is None:
            unresolved.append(dep)
            continue
        dep_roots[dep] = dep_root
        libs.append(os.path.join(dep_root, "lib"))

    meta = eco.read_meta6(root) or {}
    tests = [os.path.relpath(p, root) for p in eco.find_test_files(root)]
    lib_args = " ".join(f"-I {shlex.quote(p)}" for p in libs)

    if args.json:
        print(json.dumps({"dist": name, "version": meta.get("version"), "root": root,
                          "libs": libs, "tests": tests, "provides": sorted(meta.get("provides") or {}),
                          "deps": resolved, "unresolved": sorted(set(unresolved)),
                          "axis": eco.source_axis(root)}, indent=2, sort_keys=True))
        return 0

    print(f"# {name} {meta.get('version', '?')}   axis={eco.source_axis(root)}")
    if unresolved:
        # The sweep records this as blocked_dep and runs neither side. Here it is
        # only a warning: a suite whose missing dependency none of its test files
        # touch is still worth running.
        print(f"# UNRESOLVED dependencies: {', '.join(sorted(set(unresolved)))}")
        print("#   the sweep would call this blocked_dep; expect the files that use them to die")
    print(f"# provides: {', '.join(sorted(meta.get('provides') or {})) or '-'}")
    print(f"# {len(tests)} test file(s): {', '.join(tests) or '-'}")
    print()
    print(f"DIST={shlex.quote(root)}")
    print(f"LIBS={shlex.quote(lib_args)}")
    print(f"MUTSU={shlex.quote(os.environ.get('MUTSU_BIN', os.path.join(REPO, 'target', 'debug', 'mutsu')))}")
    print(f"RAKU={shlex.quote(os.environ.get('RAKU_BIN', 'raku'))}")
    print()
    print('# cd "$DIST" first: suites reach for fixtures by relative path.')
    if tests:
        print(f'# (cd "$DIST" && timeout 120 $RAKU   $LIBS {shlex.quote(tests[0])})')
        print(f'# (cd "$DIST" && timeout 120 $MUTSU $LIBS {shlex.quote(tests[0])})')
    return 0


def err(msg: str) -> int:
    print(msg, file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
