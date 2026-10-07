#!/usr/bin/env python3
"""Compare the `.AST` text of two implementations, statement by statement.

usage: compare.py RAKUDO_DIR MUTSU_DIR [--show CLASS [N]]

Each directory holds one `<file>.out` per sampled test, written by
`scripts/ast-text-corpus.sh run`: `@@@ <index>` then the `.raku` text of that
top-level statement. A file counts only when both sides rendered it (a file
mutsu refuses is not compared; the refusal is a different finding).

Prints the share of identical statements and the differing statements grouped by
class. A hunk of a statement is classified by the first rule in RULES that
matches (a = the rakudo side of the hunk, b = the mutsu side); a statement with
hunks of several classes counts under each, the second number is the statements
that have that class only. `--show CLASS [N]` prints N example hunks of a class.
"""
import collections
import difflib
import os
import re
import sys

RULES = [
    ("call-without-parentheses", lambda a, b: "WithoutParentheses" in a and "Call::Name" in b),
    ("block-may-have-signature", lambda a, b: "may-have-signature" in a),
    ("required-topic-bool", lambda a, b: "required-topic" in a or "required-topic" in b),
    ("literal-hash-index", lambda a, b: "LiteralHashIndex" in a),
    ("colonpair-vs-fatarrow", lambda a, b: "ColonPair" in a),
    ("language-version", lambda a, b: "LanguageVersion" in a),
    ("parenthesised-operand", lambda a, b: ("Circumfix::Parentheses" in b and "Circumfix::Parentheses" not in a)
        or ("SemiList" in b and "SemiList" in a)),
    ("whatever", lambda a, b: "Term::Whatever" in a and "WhateverCode::Argument" in b),
    ("nqp-op", lambda a, b: "RakuAST::Nqp" in a),
    ("words-quote", lambda a, b: "words val" in a and "colonpairs" not in a),
    ("attribute-var", lambda a, b: "Var::Attribute" in a),
    ("topic-call", lambda a, b: "TopicCall" in a),
    # A prefix over a bare statement (`gather say 1`): rakudo's text holds a
    # `Statement::` where mutsu's holds the block `gather { ... }` makes.
    ("bare-prefix", lambda a, b: re.search(
        r"StatementPrefix::[A-Za-z:]+\.new\(\s*RakuAST::Statement::", a) is not None
        and re.search(r"StatementPrefix::[A-Za-z:]+\.new\(\s*RakuAST::Block", b) is not None),
    ("statement-prefix", lambda a, b: "StatementPrefix" in a),
    ("mixin", lambda a, b: "Mixin" in a),
    ("heredoc", lambda a, b: "Heredoc" in a),
    ("name-parts-colonpairs", lambda a, b: "from-identifier-parts" in a or "colonpairs" in a),
    ("call-term-args", lambda a, b: "Call::Term" in a),
    ("declarator-docs", lambda a, b: "declarator-docs" in a),
    ("placeholder-layout", lambda a, b: "Placeholder" in a or "Placeholder" in b),
    ("prefix-vs-infix", lambda a, b: "ApplyPrefix" in a and "ApplyInfix" in b),
    ("postfix-vs-infix", lambda a, b: "ApplyPostfix" in a and "ApplyInfix" in b),
    ("private-method", lambda a, b: "PrivateMethod" in a),
    ("empty-array-composer", lambda a, b: "operands => ()" in b),
    ("heredoc-stop", lambda a, b: "stop " in a),
    ("index-assignee", lambda a, b: "assignee" in a),
    ("type-only-param", lambda a, b: "__type_only__" in b),
    ("item-contextualizer", lambda a, b: "Contextualizer" in a),
    ("parens-dropped", lambda a, b: "Circumfix::Parentheses" in a and "Circumfix::Parentheses" not in b),
    ("parameters-empty", lambda a, b: "parameters" in a and "parameters" in b),
]


def classify(a, b):
    for name, predicate in RULES:
        if predicate(a, b):
            return name
    return "other"


def blocks(path):
    result, current = {}, None
    with open(path, errors="replace") as handle:
        for line in handle:
            if line.startswith("@@@ "):
                current = int(line[4:])
                result[current] = []
            elif current is not None:
                result[current].append(line.rstrip("\n"))
    return result


def hunks(a, b):
    """The differing hunks of two statements as (class, rakudo lines, mutsu lines)."""
    for tag, i1, i2, j1, j2 in difflib.SequenceMatcher(None, a, b, autojunk=False).get_opcodes():
        if tag != "equal":
            yield classify(" ".join(a[i1:i2]), " ".join(b[j1:j2])), a[i1:i2], b[j1:j2]


def main():
    rk_dir, mu_dir = sys.argv[1], sys.argv[2]
    show = None
    if "--show" in sys.argv:
        at = sys.argv.index("--show")
        show = (sys.argv[at + 1], int(sys.argv[at + 2]) if len(sys.argv) > at + 2 else 6)
    files = sorted(
        name[:-4]
        for name in os.listdir(rk_dir)
        if name.endswith(".out")
        and os.path.exists(f"{mu_dir}/{name}")
        and os.path.getsize(f"{mu_dir}/{name}") > 0
    )
    total, differing, other = 0, {}, collections.Counter()
    shown = 0
    for name in files:
        rk, mu = blocks(f"{rk_dir}/{name}.out"), blocks(f"{mu_dir}/{name}.out")
        for index in sorted(set(rk) | set(mu)):
            total += 1
            a, b = rk.get(index), mu.get(index)
            if a == b:
                continue
            if a is None or b is None:
                differing[(name, index)] = {"missing-statement"}
                continue
            classes = set()
            for cls, left, right in hunks(a, b):
                classes.add(cls)
                if cls == "other":
                    key = (re.sub(r'"[^"]*"', '"S"', " ".join(left).strip())[:90],
                           re.sub(r'"[^"]*"', '"S"', " ".join(right).strip())[:90])
                    other[key] += 1
                if show and cls == show[0] and shown < show[1]:
                    shown += 1
                    print(f"--- {name} stmt {index}")
                    for line in left:
                        print(" rk", line)
                    for line in right:
                        print(" mu", line)
            differing[(name, index)] = classes
    if show:
        return
    identical = total - len(differing)
    print(f"files {len(files)}  statements {total}  identical {identical} ({100.0 * identical / total:.1f}%)")
    count = collections.Counter(c for classes in differing.values() for c in classes)
    only = collections.Counter(next(iter(c)) for c in differing.values() if len(c) == 1)
    for cls, n in count.most_common():
        print("%-26s %7d %7d" % (cls, n, only.get(cls, 0)))
    for key, n in other.most_common(15):
        print(n, key)


main()
