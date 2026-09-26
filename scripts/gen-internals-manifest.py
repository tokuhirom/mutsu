#!/usr/bin/env python3
"""Generate the data behind the site's Internals section.

The Internals pages describe how mutsu works inside. The parts of that
description that are *lists* -- every VM opcode, every tag a `Value` word can
carry, every built-in type's ancestry -- would drift the week they were
written by hand (AGENTS.md's own "~100 opcodes" sat stale for months while the
set grew past 340), so they are generated from the places that are
authoritative by construction:

  site/content/opcodes.json (site/opcodes.html)
    - src/opcode.rs, `enum OpCode`: each variant, its operands, the `///` doc
      comment above it, and the `// -- Section --` comment it sits under;
    - src/vm/vm_exec_dispatch.rs, `exec_one_dispatch`: the `// Cost:` line
      AGENTS.md requires above every `OpCode::` arm
      (docs/complexity-annotations.md).

  site/content/types.json (site/types.html)
    - src/value/nanbox/mod.rs, `enum Kind` and `payload_op`: every tag a
      NaN-boxed `Value` word can carry, and whether its payload is inline, an
      `Arc<T>`, a cycle-collected `Gc<T>` or a `WeakGc<T>` -- read from the
      match that actually bumps and releases it, not from a comment;
    - src/builtins/builtin_type_catalog.rs, `CATALOG`: the built-in types'
      MROs and roles, captured from Rakudo's own `.^mro`.

Neither output is committed (both are git-ignored, like content/stats.json).
pages.yml runs this at deploy time, and ci.yml runs it before the site's e2e
test, so the published pages always describe the commit they were built from:

    python3 scripts/gen-internals-manifest.py            # write the JSON
    python3 scripts/gen-internals-manifest.py --summary  # also print gaps

The parsers are deliberately line-based and keyed on the files' fixed layout
(variants at 4-space indent inside the enum, dispatch arms at 12-space indent
inside the `match`). If that layout changes, the sanity checks in `main` fail
the run loudly instead of publishing an empty or truncated page.
"""

from __future__ import annotations

import json
import os
import re
import subprocess
import sys

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OPCODE_RS = "src/opcode.rs"
DISPATCH_RS = "src/vm/vm_exec_dispatch.rs"
NANBOX_RS = "src/value/nanbox/mod.rs"
CATALOG_RS = "src/builtins/builtin_type_catalog.rs"
CONTENT_DIR = os.path.join(REPO_ROOT, "site", "content")
ISSUES_URL = "https://github.com/tokuhirom/mutsu/issues"

# Fewer variants than this means the parser lost track of the enum, not that
# the instruction set shrank by half; refuse to publish that.
MIN_EXPECTED_OPS = 200
MIN_EXPECTED_KINDS = 40
MIN_EXPECTED_TYPES = 50

SECTION_RE = re.compile(r"^    // -- (.+?) --\s*$")
VARIANT_RE = re.compile(r"^    ([A-Z][A-Za-z0-9_]*)\s*([({,]|$)")
FIELD_RE = re.compile(r"^\s*(?:pub(?:\([^)]*\))?\s+)?([a-z_][a-z0-9_]*)\s*:\s*(.+?),?\s*$")
ARM_RE = re.compile(r"^            OpCode::([A-Za-z0-9_]+)")
ARM_NAMES_RE = re.compile(r"OpCode::([A-Za-z0-9_]+)")


def read_lines(rel: str) -> list[str]:
    with open(os.path.join(REPO_ROOT, rel), encoding="utf-8") as f:
        return f.read().split("\n")


def clean_doc(lines: list[str]) -> str:
    """Turn rustdoc-flavoured Markdown into the subset the site renders.

    Intra-doc links (``[`Foo::bar`]``, ``[`x`](Self::y)``) resolve only inside
    rustdoc; on the site they would print as literal brackets, so they become
    plain code spans. Links to real URLs are kept.
    """
    text = "\n".join(lines).strip("\n")
    # [`X`](target) where target is not a URL -> `X`
    text = re.sub(r"\[(`[^`\n]+`)\]\((?!https?:)[^)\s]*\)", r"\1", text)
    # [text](target) where target is not a URL -> text
    text = re.sub(r"\[([^\]\n]+)\]\((?!https?:)[^)\s]*\)", r"\1", text)
    # [`X`] (reference-style intra-doc link) -> `X`
    text = re.sub(r"\[(`[^`\n]+`)\](?![(\[])", r"\1", text)
    # a bare issue reference (#8695) becomes a link to the issue
    text = re.sub(r"(?<![\w`&/\[])#(\d{3,5})\b", rf"[#\1]({ISSUES_URL}/\1)", text)
    return text


def strip_comment(line: str, prefix: str) -> str:
    body = line.strip()[len(prefix):]
    return body[1:] if body.startswith(" ") else body


def parse_payload(text: str) -> dict:
    """Describe a variant's operands from its source text (after the name)."""
    text = text.strip().rstrip(",").strip()
    if not text:
        return {"kind": "unit"}
    if text.startswith("("):
        inner = text[1:text.rfind(")")]
        return {"kind": "tuple", "types": split_top_level(inner)}
    # struct variant: parse field by field so each keeps its own doc comment
    fields = []
    doc: list[str] = []
    depth = 0
    pending = ""
    for raw in text[1:text.rfind("}")].split("\n"):
        line = raw.strip()
        if not line:
            continue
        if depth == 0 and not pending and line.startswith("///"):
            doc.append(strip_comment(line, "///"))
            continue
        if depth == 0 and not pending and line.startswith("//"):
            continue
        pending = f"{pending} {line}".strip() if pending else line
        depth += sum(line.count(c) for c in "(<[{") - sum(line.count(c) for c in ")>]}")
        # `->` in a fn type is not a closing angle bracket
        depth += line.count("->")
        if depth <= 0 and pending.endswith(","):
            m = FIELD_RE.match(pending)
            if m:
                fields.append({"name": m.group(1), "type": m.group(2).strip(),
                               "doc": clean_doc(doc)})
            doc, pending, depth = [], "", 0
    if pending:
        m = FIELD_RE.match(pending + ",")
        if m:
            fields.append({"name": m.group(1), "type": m.group(2).strip(),
                           "doc": clean_doc(doc)})
    return {"kind": "struct", "fields": fields}


def split_top_level(s: str) -> list[str]:
    out, depth, cur = [], 0, ""
    for ch in s:
        if ch in "(<[{":
            depth += 1
        elif ch in ")>]}":
            depth -= 1
        if ch == "," and depth == 0:
            if cur.strip():
                out.append(" ".join(cur.split()))
            cur = ""
        else:
            cur += ch
    if cur.strip():
        out.append(" ".join(cur.split()))
    return out


def parse_opcodes() -> list[dict]:
    lines = read_lines(OPCODE_RS)
    start = next(i for i, l in enumerate(lines) if re.match(r"^pub\(crate\) enum OpCode\s*\{", l))
    ops = []
    section = "(uncategorized)"
    doc: list[str] = []
    note: list[str] = []
    i = start + 1
    while i < len(lines):
        line = lines[i]
        if line.startswith("}"):
            break
        m = SECTION_RE.match(line)
        if m:
            section = m.group(1).strip()
            doc, note = [], []
            i += 1
            continue
        stripped = line.strip()
        if stripped.startswith("///") and line.startswith("    ///"):
            doc.append(strip_comment(line, "///"))
            i += 1
            continue
        if stripped.startswith("//") and line.startswith("    //"):
            note.append(strip_comment(line, "//"))
            i += 1
            continue
        if stripped.startswith("#["):
            i += 1
            continue
        if not stripped:
            # a blank line ends a floating comment that documented nothing
            doc, note = [], []
            i += 1
            continue
        m = VARIANT_RE.match(line)
        if m:
            name, opener = m.group(1), m.group(2)
            def_line = i + 1
            body = line[line.index(name) + len(name):]
            if opener in ("(", "{"):
                close = ")" if opener == "(" else "}"
                depth = body.count(opener) - body.count(close)
                while depth > 0:
                    i += 1
                    body += "\n" + lines[i]
                    depth += lines[i].count(opener) - lines[i].count(close)
            ops.append({
                "name": name,
                "category": section,
                # `///` is the documentation; a plain `//` block directly above a
                # variant is a developer note, shown only when there is no doc.
                "doc": clean_doc(doc) if doc else clean_doc(note),
                "doc_kind": "doc" if doc else ("note" if note else "none"),
                "payload": parse_payload(body),
                "def_line": def_line,
            })
            doc, note = [], []
        i += 1
    return ops


def parse_dispatch() -> dict[str, dict]:
    lines = read_lines(DISPATCH_RS)
    arms: dict[str, dict] = {}
    for i, line in enumerate(lines):
        if not ARM_RE.match(line):
            continue
        head = line.split("=>")[0]
        names = ARM_NAMES_RE.findall(head)
        # the comment block immediately above the arm
        j = i - 1
        block: list[str] = []
        while j >= 0 and lines[j].strip().startswith("//"):
            block.insert(0, strip_comment(lines[j], "//"))
            j -= 1
        cost = None
        for k, text in enumerate(block):
            if text.startswith("Cost:"):
                cost = " ".join([text[len("Cost:"):].strip()] + [t.strip() for t in block[k + 1:]])
                break
        for name in names:
            arms.setdefault(name, {"cost": cost, "dispatch_line": i + 1})
    return arms


def parse_kinds() -> list[dict]:
    """Every `Kind` a NaN-boxed word can carry, with its payload storage.

    The storage class comes from `payload_op`, the match that bumps and
    releases the payload on Clone/Drop: `arc_op::<T>`, `gc_op::<T>` or
    `weak_op::<T>`, and the `payload_free_kinds!()` macro for inline kinds.
    The `// -- ... --` group comments in the enum are not trusted for this.
    """
    lines = read_lines(NANBOX_RS)
    start = next(i for i, l in enumerate(lines) if re.match(r"^pub\(in crate::value\) enum Kind\s*\{", l))
    kinds = []
    doc: list[str] = []
    for i in range(start + 1, len(lines)):
        line = lines[i]
        if line.startswith("}"):
            break
        stripped = line.strip()
        if stripped.startswith("///"):
            doc.append(strip_comment(line, "///"))
            continue
        m = re.match(r"^    ([A-Z][A-Za-z0-9]*)\s*(=\s*\d+)?,", line)
        if m:
            kinds.append({"name": m.group(1), "doc": clean_doc(doc), "def_line": i + 1})
        doc = []

    text = "\n".join(lines)
    body = text[text.index("unsafe fn payload_op("):]
    body = body[:body.index("\n}\n")]
    storage: dict[str, tuple[str, str]] = {}
    # each arm: `Kind::A | Kind::B => arc_op::<T>(bits, op)` (possibly braced)
    for arm in re.finditer(r"((?:\|?\s*Kind::\w+\s*)+)=>\s*\{?\s*(arc|gc|weak)_op::<(.+?)>\(bits", body, re.S):
        for name in re.findall(r"Kind::(\w+)", arm.group(1)):
            storage[name] = (arm.group(2), re.sub(r"crate::(?:value::|rakuast::)?", "", " ".join(arm.group(3).split())))
    free_m = re.search(r"macro_rules! payload_free_kinds \{.*?\n\}", text, re.S)
    free = set(re.findall(r"Kind::(\w+)", free_m.group(0))) if free_m else set()
    for k in kinds:
        if k["name"] in storage:
            k["storage"], k["payload"] = storage[k["name"]]
        elif k["name"] in free:
            k["storage"], k["payload"] = "inline", None
        else:
            k["storage"], k["payload"] = None, None
    return kinds


def parse_type_catalog() -> list[dict]:
    lines = read_lines(CATALOG_RS)
    text = "\n".join(lines)
    start = text.index("static CATALOG:")
    end = text.index("\n];", start)
    rows = []
    row_re = re.compile(
        r'row!\(\s*"([^"]+)",\s*mro:\s*\[((?:\s*"[^"]*"\s*,?)*)\s*\],'
        r'\s*roles:\s*\[((?:\s*"[^"]*"\s*,?)*)\s*\],\s*owner:\s*"([^"]*)"',
        re.S)
    for m in row_re.finditer(text, start, end):
        rows.append({
            "name": m.group(1),
            "mro": re.findall(r'"([^"]*)"', m.group(2)),
            "roles": re.findall(r'"([^"]*)"', m.group(3)),
            "def_line": text.count("\n", 0, m.start()) + 1,
        })
    declared = len(re.findall(r"\brow!\(", text[start:end]))
    if declared != len(rows):
        raise SystemExit(f"gen-internals-manifest: parsed {len(rows)} of the {declared} "
                         f"row!(...) entries in {CATALOG_RS}; has the row syntax changed?")
    return rows


def git_commit() -> str | None:
    env = os.environ.get("GITHUB_SHA")
    if env:
        return env
    try:
        return subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=REPO_ROOT,
                                       text=True, stderr=subprocess.DEVNULL).strip()
    except (OSError, subprocess.CalledProcessError):
        return None


def write_json(name: str, data: dict) -> None:
    os.makedirs(CONTENT_DIR, exist_ok=True)
    with open(os.path.join(CONTENT_DIR, name), "w", encoding="utf-8") as f:
        json.dump(data, f, ensure_ascii=False, indent=1)
        f.write("\n")


def build_opcodes(problems: list[str]) -> dict:
    ops = parse_opcodes()
    arms = parse_dispatch()
    for op in ops:
        arm = arms.get(op["name"])
        op["cost"] = arm["cost"] if arm else None
        op["dispatch_line"] = arm["dispatch_line"] if arm else None

    categories: list[dict] = []
    for op in ops:
        if not categories or categories[-1]["name"] != op["category"]:
            # a section name can recur; merge the later block into the first
            existing = next((c for c in categories if c["name"] == op["category"]), None)
            if existing is None:
                categories.append({"name": op["category"], "ops": []})
            else:
                categories.append(categories.pop(categories.index(existing)))
        categories[-1]["ops"].append(op["name"])

    names = [op["name"] for op in ops]
    dupes = sorted({n for n in names if names.count(n) > 1})
    if len(ops) < MIN_EXPECTED_OPS:
        problems.append(f"only {len(ops)} OpCode variants parsed from {OPCODE_RS} "
                        f"(expected at least {MIN_EXPECTED_OPS}); has the enum's layout changed?")
    if dupes:
        problems.append(f"duplicate OpCode variants parsed: {', '.join(dupes)}")
    stray = sorted(set(arms) - set(names))
    if stray:
        problems.append(f"dispatch arms name unknown opcodes: {', '.join(stray)}")
    return {
        "commit": git_commit(),
        "sources": {"opcodes": OPCODE_RS, "dispatch": DISPATCH_RS},
        "counts": {
            "ops": len(ops),
            "categories": len(categories),
            "documented": sum(1 for op in ops if op["doc_kind"] == "doc"),
            "with_cost": sum(1 for op in ops if op["cost"]),
        },
        "categories": categories,
        "ops": ops,
    }


def build_types(problems: list[str]) -> dict:
    kinds = parse_kinds()
    types = parse_type_catalog()
    if len(kinds) < MIN_EXPECTED_KINDS:
        problems.append(f"only {len(kinds)} Kind variants parsed from {NANBOX_RS}")
    unmapped = [k["name"] for k in kinds if k["storage"] is None]
    if unmapped:
        problems.append(f"Kind variants with no payload_op arm: {', '.join(unmapped)}")
    if len(types) < MIN_EXPECTED_TYPES:
        problems.append(f"only {len(types)} rows parsed from {CATALOG_RS}")
    bad = [t["name"] for t in types if not t["mro"] or t["mro"][0] != t["name"] or t["mro"][-1] != "Mu"]
    if bad:
        problems.append(f"catalog rows whose mro does not run from the type itself to Mu: {', '.join(bad)}")
    return {
        "commit": git_commit(),
        "sources": {"kinds": NANBOX_RS, "catalog": CATALOG_RS},
        "kinds": kinds,
        "types": types,
    }


def main() -> int:
    problems: list[str] = []
    opcodes = build_opcodes(problems)
    types = build_types(problems)
    if problems:
        for p in problems:
            print(f"gen-internals-manifest: {p}", file=sys.stderr)
        return 1
    write_json("opcodes.json", opcodes)
    write_json("types.json", types)

    c = opcodes["counts"]
    print(f"opcodes.json: {c['ops']} opcodes in {c['categories']} families, "
          f"{c['documented']} documented, {c['with_cost']} with a Cost line")
    print(f"types.json: {len(types['kinds'])} value kinds, {len(types['types'])} catalogued built-in types")
    if "--summary" in sys.argv[1:]:
        missing = [op["name"] for op in opcodes["ops"] if op["dispatch_line"] is None]
        if missing:
            print(f"no exec_one_dispatch arm: {', '.join(missing)}")
        nocost = [op["name"] for op in opcodes["ops"] if op["dispatch_line"] and not op["cost"]]
        if nocost:
            print(f"dispatch arm without a Cost line: {', '.join(nocost)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
