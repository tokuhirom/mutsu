#!/usr/bin/env python3
"""Generate wasm-demo/content/batteries.json from the vendored bundle.

The batteries page on the site (wasm-demo/batteries.html) lists every library
mutsu ships in its `modules/` tree and renders each one's upstream README. That
list must never drift from what is actually bundled, so it is *generated* from
the authoritative source — each `modules/<Dist>/META6.json` — rather than
hand-written. Metadata (version, license, provided modules) comes straight from
META6.json; the documentation body is the vendored `README.md`.

Run by hand after (re-)vendoring a module, and again at deploy time in
pages.yml, so the committed snapshot and the deployed page stay in step:

    python3 scripts/gen-batteries-manifest.py

A small per-module sidecar map below carries the facts META6.json does not: the
battery "slot" this module fills, the path to its mutsu selection record under
docs/batteries/, and -- only where META6.json is silent or wrong -- the license
the dist actually ships under. Add an entry when you bundle a new library.
"""

from __future__ import annotations

import json
import os
import sys

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
MODULES_DIR = os.path.join(REPO_ROOT, "modules")
OUT_PATH = os.path.join(REPO_ROOT, "wasm-demo", "content", "batteries.json")

# Facts that META6.json does not carry. Keyed by META6 "name". Optional — a
# module missing from this map still lists, just without a slot/record link.
SIDECAR = {
    "OpenSSL": {
        "slot": "TLS / HTTPS socket (foundation)",
        "record": "docs/batteries/tls-openssl.md",
    },
    "IO::Socket::SSL": {
        "slot": "TLS / HTTPS socket (foundation)",
        "record": "docs/batteries/tls-openssl.md",
    },
    "URI": {
        "slot": "HTTP client dependency layer",
        "record": "docs/batteries/http-deps.md",
    },
    "MIME::Base64": {
        "slot": "HTTP client dependency layer",
        "record": "docs/batteries/http-deps.md",
    },
    "HTTP::Status": {
        "slot": "HTTP client dependency layer",
        "record": "docs/batteries/http-deps.md",
        # META6.json says NOASSERTION; the README states Artistic-2.0.
        "license": "Artistic-2.0",
    },
    "DateTime::Parse": {
        "slot": "HTTP client dependency layer",
        "record": "docs/batteries/http-deps.md",
        # META6.json has no license key; the dist ships an MIT LICENSE file.
        "license": "MIT",
    },
    "Encode": {
        "slot": "HTTP client dependency layer",
        "record": "docs/batteries/http-deps.md",
        # Upstream states no license anywhere; clarification is pending at
        # https://github.com/sergot/perl6-encode/issues/17. Say so on the page
        # rather than leaving the chip blank (BATTERIES.md 4).
        "license": "license pending",
    },
    "File::Temp": {
        "slot": "HTTP client dependency layer",
        "record": "docs/batteries/http-deps.md",
    },
    "File::Directory::Tree": {
        "slot": "HTTP client dependency layer",
        "record": "docs/batteries/http-deps.md",
    },
    "HTTP::UserAgent": {
        "slot": "HTTP client",
        "record": "docs/batteries/http-client.md",
    },
    "Test::Util::ServerPort": {
        "slot": "Test helpers",
        "record": "docs/batteries/test-helpers.md",
    },
    "Template::Mustache": {
        "slot": "Template engine",
        "record": "docs/batteries/templates.md",
    },
}


def read_text(path: str) -> str:
    try:
        with open(path, encoding="utf-8") as fh:
            return fh.read()
    except OSError:
        return ""


def upstream_url(meta: dict) -> str:
    url = meta.get("source-url") or meta.get("support", {}).get("source", "")
    if url.endswith(".git"):
        url = url[:-4]
    return url


def collect() -> list[dict]:
    libraries = []
    for entry in sorted(os.listdir(MODULES_DIR)):
        dist_dir = os.path.join(MODULES_DIR, entry)
        meta_path = os.path.join(dist_dir, "META6.json")
        if not os.path.isfile(meta_path):
            continue
        meta = json.loads(read_text(meta_path))
        name = meta.get("name", entry)
        sidecar = SIDECAR.get(name, {})
        libraries.append(
            {
                "name": name,
                "version": meta.get("version", ""),
                "description": meta.get("description", ""),
                "license": sidecar.get("license") or meta.get("license") or "",
                "authors": meta.get("authors") or ([meta["author"]] if meta.get("author") else []),
                "auth": meta.get("auth", ""),
                "provides": sorted((meta.get("provides") or {}).keys()),
                "depends": meta.get("depends") or [],
                "upstream": upstream_url(meta),
                "slot": sidecar.get("slot", ""),
                "record": sidecar.get("record", ""),
                "readme": read_text(os.path.join(dist_dir, "README.md")),
            }
        )
    # Bundle order is bottom-up (a foundation before what stands on it): a
    # module with no bundled dependency sorts before one that depends on it.
    bundled = {lib["name"] for lib in libraries}

    by_name = {lib["name"]: lib for lib in libraries}

    def depth(lib: dict, seen: frozenset = frozenset()) -> int:
        d = 0
        for dep in lib["depends"]:
            dep_name = dep.split(":")[0]
            child = by_name.get(dep_name)
            if child is not None and dep_name not in seen:
                d = max(d, 1 + depth(child, seen | {lib["name"]}))
        return d

    libraries.sort(key=lambda lib: (depth(lib), lib["name"]))
    return libraries


def main() -> int:
    if not os.path.isdir(MODULES_DIR):
        sys.stderr.write(f"no modules/ dir at {MODULES_DIR}\n")
        return 1
    manifest = {"libraries": collect()}
    os.makedirs(os.path.dirname(OUT_PATH), exist_ok=True)
    with open(OUT_PATH, "w", encoding="utf-8") as fh:
        json.dump(manifest, fh, ensure_ascii=False, indent=2)
        fh.write("\n")
    rel = os.path.relpath(OUT_PATH, REPO_ROOT)
    print(f"wrote {rel}: {len(manifest['libraries'])} bundled libraries")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
