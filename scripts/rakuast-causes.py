#!/usr/bin/env python3
"""Summarise tmp/rakuast-causes/results.tsv (see `scripts/rakuast-frontend.sh causes`).

Prints how many files PASS / DIFF / REFUSE, then the REFUSE rows by cause. A
cause is the refusal text with its numbers erased; the refusals that name an
internal AST node dump (`BareWord("x")`, `SyntheticBlock([...])`) are grouped by
the node's kind, and `desugared construct` / `literal` ones by what they name.
"""
import collections
import re
import sys

PREFIX = "RakuAST: `.AST` does not yet support this construct: "


def cause(message: str) -> str:
    if message.startswith("RakuAST: EVAL does not yet support lowering"):
        return "lowering " + message.split("`")[1]
    if not message.startswith(PREFIX):
        return "other: " + message[:60]
    text = message[len(PREFIX):]
    m = re.match(r"desugared construct \(internal name `([^`]*)`", text)
    if m:
        return "desugared: " + re.sub(r"\d+", "N", m.group(1))
    m = re.match(r"literal (\w+)", text)
    if m:
        return "literal " + m.group(1)
    m = re.match(r"([A-Z][A-Za-z]+)[({ ]", text)
    if m and not text.startswith(("Class", "Role")):
        return "node " + m.group(1)
    return re.sub(r"\d+", "N", text)[:70]


def main(path: str) -> None:
    counts = collections.Counter()
    causes = collections.Counter()
    for line in open(path, encoding="utf-8"):
        fields = line.rstrip("\n").split("\t")
        counts[fields[0]] += 1
        if fields[0] == "REFUSE" and len(fields) > 2:
            causes[cause(fields[2])] += 1
    print("  ".join(f"{kind} {n}" for kind, n in sorted(counts.items())))
    for name, n in causes.most_common(60):
        print(f"{n:5}  {name}")


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "tmp/rakuast-causes/results.tsv")
