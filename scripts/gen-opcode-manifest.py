#!/usr/bin/env python3
"""Generate site/content/opcodes.json: the VM instruction-set reference.

The site's Internals section (site/opcodes.html) lists every bytecode
instruction the VM executes. A hand-written list of ~370 opcodes would drift
the week it was written (CLAUDE.md's own "~100 opcodes" sat stale for months
while the set grew past 340), so the reference is *generated* from the two
places that are authoritative by construction:

  - src/opcode.rs, `enum OpCode`: each variant, its operands, the `///` doc
    comment above it, and the `// -- Section --` comment it sits under;
  - src/vm/vm_exec_dispatch.rs, `exec_one_dispatch`: the `// Cost:` line
    CLAUDE.md requires above every `OpCode::` arm (docs/complexity-annotations.md).

The output is not committed (it is git-ignored, like content/stats.json).
pages.yml runs this at deploy time, and ci.yml runs it before the site's e2e
test, so the published reference always describes the commit it was built
from:

    python3 scripts/gen-opcode-manifest.py            # write the JSON
    python3 scripts/gen-opcode-manifest.py --summary  # also print coverage

The parser is deliberately line-based and keyed on the files' fixed layout
(variants at 4-space indent inside the enum, dispatch arms at 12-space indent
inside the `match`). If that layout changes, the sanity checks at the bottom
fail the run loudly instead of publishing an empty or truncated page.
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
OUT_PATH = os.path.join(REPO_ROOT, "site", "content", "opcodes.json")
ISSUES_URL = "https://github.com/tokuhirom/mutsu/issues"

# Fewer variants than this means the parser lost track of the enum, not that
# the instruction set shrank by half; refuse to publish that.
MIN_EXPECTED_OPS = 200

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


def git_commit() -> str | None:
    env = os.environ.get("GITHUB_SHA")
    if env:
        return env
    try:
        return subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=REPO_ROOT,
                                       text=True, stderr=subprocess.DEVNULL).strip()
    except (OSError, subprocess.CalledProcessError):
        return None


def main() -> int:
    ops = parse_opcodes()
    arms = parse_dispatch()
    for op in ops:
        arm = arms.get(op["name"])
        op["cost"] = arm["cost"] if arm else None
        op["dispatch_line"] = arm["dispatch_line"] if arm else None

    categories: list[dict] = []
    for op in ops:
        if not categories or categories[-1]["name"] != op["category"]:
            # a section name can recur (two `-- Loops --` blocks); merge them
            existing = next((c for c in categories if c["name"] == op["category"]), None)
            if existing is None:
                categories.append({"name": op["category"], "ops": []})
            else:
                categories.append(categories.pop(categories.index(existing)))
        categories[-1]["ops"].append(op["name"])

    names = [op["name"] for op in ops]
    dupes = sorted({n for n in names if names.count(n) > 1})
    problems = []
    if len(ops) < MIN_EXPECTED_OPS:
        problems.append(f"only {len(ops)} OpCode variants parsed from {OPCODE_RS} "
                        f"(expected at least {MIN_EXPECTED_OPS}); has the enum's layout changed?")
    if dupes:
        problems.append(f"duplicate variant names parsed: {', '.join(dupes)}")
    stray = sorted(set(arms) - set(names))
    if stray:
        problems.append(f"dispatch arms name unknown opcodes: {', '.join(stray)}")
    if problems:
        for p in problems:
            print(f"gen-opcode-manifest: {p}", file=sys.stderr)
        return 1

    manifest = {
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
    os.makedirs(os.path.dirname(OUT_PATH), exist_ok=True)
    with open(OUT_PATH, "w", encoding="utf-8") as f:
        json.dump(manifest, f, ensure_ascii=False, indent=1)
        f.write("\n")

    c = manifest["counts"]
    print(f"opcodes.json: {c['ops']} opcodes in {c['categories']} categories, "
          f"{c['documented']} documented, {c['with_cost']} with a Cost line")
    if "--summary" in sys.argv[1:]:
        missing = [op["name"] for op in ops if op["dispatch_line"] is None]
        if missing:
            print(f"no exec_one_dispatch arm: {', '.join(missing)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
