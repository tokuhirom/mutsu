#!/usr/bin/env python3
"""ecosystem-tickets.py — turn the parity ledger into an impact-ordered root-cause queue.

`ecosystem/dists/**.json` records *what* fails; this reads them back and answers
*why*, grouped. Every actionable failure site in the ledger -- a `blocked_load`
module error, and the `first_failure` of every `regression`/`partial` test file --
is normalised into a root-cause signature, and signatures are clustered so that
one cluster is one interpreter fix. Clusters are ordered by the number of
DISTRIBUTIONS they affect, which is the only impact metric that means anything
here: a message repeated across 40 modules of one distribution is one bug worth
one distribution, and the ledger's raw per-module counts say otherwise.

This is phase P5 of ADR-0085. The output is meant to be filed as GitHub issues
(docs/issue-workflow.md), NOT committed as a queue file: a generated queue over
~1600 records would conflict on every sweep, and an issue number keeps resolving
after the finding is fixed where a path in a generated file does not.

    scripts/ecosystem-tickets.py                      # the table, biggest first
    scripts/ecosystem-tickets.py --min-dists 1 --all  # the whole tail
    scripts/ecosystem-tickets.py --family no-such-method
    scripts/ecosystem-tickets.py --issue 3f0a1c2d     # a ready-to-file issue body
    scripts/ecosystem-tickets.py --json tmp/eco-tickets.json
    scripts/ecosystem-tickets.py --self-test

A cluster's `id` is a digest of its signature, so it is stable across sweeps: the
issue body carries `eco-cluster: <id>`, and re-running this after the next sweep
tells you whether a cluster is new by searching the tracker for that string
rather than by keeping state in the repository.
"""

from __future__ import annotations

import argparse
import collections
import hashlib
import json
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ecosystem_common as eco  # noqa: E402

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DISTS_DIR = os.path.join(REPO, "ecosystem", "dists")

# Sites the ledger records but which are not mutsu's to fix. Both are measured
# facts about the machine that ran the sweep, not about the interpreter, and both
# are small enough to enumerate: keeping them in the queue would put a ticket on
# the board that no interpreter change can close. `--class all` shows them.
NOISE = (
    (re.compile(r"failed to lookup address information|Temporary failure in name resolution"),
     "network"),
    (re.compile(r"failed to spawn worker thread.*WouldBlock"),
     "host-resources"),
)


def _q(s: str) -> str:
    """Collapse quoted payloads and bare numbers -- the volatile part of a message."""
    s = re.sub(r"(\w)'(\w)", r"\1\2", s)          # don't -> dont, so quotes pair up
    s = re.sub(r"'[^']*'", "'X'", s)
    s = re.sub(r'"[^"]*"', '"X"', s)
    s = re.sub(r"\bline \d+\b", "line N", s)
    s = re.sub(r"\b\d+\b", "N", s)
    return re.sub(r"\s+", " ", s).strip()


def _expectation_dump(s: str) -> str | None:
    """mutsu's parser failure message is a dump of what it would have accepted.

    It names the *file and line* (and a caret line under it), which is why the
    harness now records the location -- but it never names the construct it
    choked on, and the expectation set it prints instead differs with parser
    state rather than with the cause. So these cluster as ONE family: every
    member is a distinct unknown parse gap, and none of them can be told apart
    from its message. They are a sampling job, not one fix.
    """
    if "expected statement:" not in s and not s.startswith("Confused."):
        return None
    return "parse error described only by the parser's expectation set"


# (family, pattern, signature template). First match wins, so order matters.
# A template of None means "cluster at the family level": the members share one
# root cause by construction, and one ticket fixes all of them.
RULES: list[tuple[str, re.Pattern, str | None]] = [
    # --- one root cause per family ------------------------------------------
    ("unknown-attribute-trait",
     re.compile(r"unknown trait '(?P<t>[^']+)' -> '(?P<n>[^']+)' in an attribute declaration"),
     None),
    ("invalid-typename",
     re.compile(r"Invalid typename '(?P<n>[^']+)'"), None),
    ("inherit-unknown-type",
     re.compile(r"'(?P<c>[^']+)' cannot inherit from '(?P<p>[^']+)' because it is unknown"), None),
    ("no-self-available",
     re.compile(r"Variable (?P<v>[$@%&][.!]\S+) used where no 'self' is available"), None),
    ("duplicate-composed-attribute",
     re.compile(r"Trait::Duplicate: attribute '(?P<a>[^']+)' already exists in class"), None),
    ("augment-missing-class",
     re.compile(r"You tried to augment class (?P<c>\S+), but it does not exist"), None),
    ("nil-string-context",
     re.compile(r"Use of Nil in string context"), None),
    ("stack-overflow",
     re.compile(r"has overflowed its stack"), None),
    ("timeout",
     re.compile(r"^SWEEP-TIMEOUT$"), None),
    ("check-phaser",
     re.compile(r"An exception occurred while evaluating a CHECK"), None),
    # --- one root cause per named payload -----------------------------------
    ("no-such-method",
     re.compile(r"No such method '(?P<m>[^']+)' for invocant of type '(?P<t>[^']+)'"),
     "No such method '{m}' on {t}"),
    ("no-such-private-method",
     re.compile(r"No such private method '(?P<m>[^']+)' for invocant of type '(?P<t>[^']+)'"),
     "No such private method '{m}' on {t}"),
    ("nqp-op",
     re.compile(r"Unsupported nqp:: op: (?P<op>nqp::\w+)"), "Unsupported {op}"),
    ("unknown-function",
     re.compile(r"Unknown function: (?P<f>\S+)"), "Unknown function: {f}"),
    ("missing-module",
     re.compile(r"Could not find (?P<m>\S+) in:"), "Could not find {m}"),
    ("import-tag",
     re.compile(r"Error while importing from '(?P<m>[^']+)': no such tag '(?P<t>[^']+)' declared"),
     "no such import tag '{t}' in {m}"),
    ("role-instantiation",
     re.compile(r"Could not instantiate role '[^']+' because it died with (?P<x>[\w:]+)"),
     "role instantiation dies with {x}"),
    ("slang",
     re.compile(r"slang activation for '[^']+' failed: (?P<why>.*)"), "slang activation: {why}"),
    ("unknown-role",
     re.compile(r"Unknown role: (?P<r>\S+)"), "Unknown role: {r}"),
]


def classify(message: str) -> tuple[str, str | None, str]:
    """(family, signature, variant) for one failure message.

    A signature of `None` means "this message carries no shared root cause": the
    caller keys the cluster per distribution instead, so that a hundred unrelated
    bugs cannot pile into one mega-ticket that no single fix closes.

    The `variant` is the identifying payload the rule matched -- which trait,
    which typename, which parent class. For a family-level cluster that is the
    whole of what differs between its members, so it must NOT be normalised away
    the way the signature is.
    """
    s = message.strip()
    # `... [at lib/A.rakumod:50]` -- the location the harness appends so that a
    # record can be triaged. It is per-record by construction, so clustering on
    # it would give one cluster per distribution; the sites below keep it.
    s = re.sub(r"\s*\[at [^\]]+\]$", "", s)
    # A parse failure names the module it was compiling; the module name is the
    # dist, not the cause, so strip it before matching.
    s = re.sub(r"^(Runtime error: )?Failed to parse module '[^']*':\s*", "", s)
    s = re.sub(r"^Runtime error:\s*", "", s)

    dump = _expectation_dump(s)
    if dump:
        return "parse-error-expectation-dump", dump, _q(s)[:100]
    # `not ok 7 -` with nothing after the dash: an *unnamed* assertion computed
    # the wrong value. The message is the whole of what the ledger knows, and it
    # is the same string for every such test in the corpus, so clustering on it
    # would merge two dozen unrelated wrong answers.
    if re.fullmatch(r"not ok\s+\d+\s*-?\s*", s):
        return "wrong-answer-unnamed-assertion", None, s
    for family, pattern, template in RULES:
        m = pattern.search(s)
        if m:
            variant = " -> ".join(v for v in m.groupdict().values() if v) or _q(s)[:100]
            if template is None:
                return family, family, variant
            return family, template.format(**m.groupdict()), variant
    return "other", _q(s)[:110], _q(s)[:100]


def noise_class(message: str) -> str | None:
    for pattern, name in NOISE:
        if pattern.search(message):
            return name
    return None


def sites(records):
    """Every actionable failure site in the ledger.

    `raku_also_fails` load entries and `no_baseline` files are excluded here for
    the same reason they are excluded from the KPI: rakudo does not pass them
    either, so they are not mutsu's to fix (ADR-0085 D3).
    """
    for r in records:
        for module, verdict in sorted((r.get("load") or {}).items()):
            if verdict in ("ok", "raku_also_fails"):
                continue
            yield r, f"load:{module}", verdict, 0
        for f in r.get("files") or []:
            if f.get("cmp") not in ("regression", "partial"):
                continue
            message = (f.get("mutsu") or {}).get("first_failure")
            if not message:
                continue
            # rakudo's own `ok` count on this file is exactly what the KPI's
            # assertion_parity gains if the cluster is fixed.
            yield r, f"file:{f['path']}", message, (f.get("raku") or {}).get("ok") or 0


def cluster(records, *, want_class="actionable"):
    out: dict[str, dict] = {}
    for record, where, message, assertions in sites(records):
        noise = noise_class(message)
        if want_class == "actionable" and noise:
            continue
        if noise:
            family, signature, variant = "noise-" + noise, noise, noise
        else:
            family, signature, variant = classify(message)
        if signature is None:
            signature = f"{family} [{record['dist']}]"
        c = out.setdefault(signature, {
            "id": hashlib.sha256(signature.encode()).hexdigest()[:8],
            "family": family,
            "signature": signature,
            "dists": set(),
            "blocked_load_dists": set(),
            "files": 0,
            "assertions": 0,
            "axis_dists": {},
            "variant_dists": {},
            "sites": [],
        })
        c["dists"].add(record["dist"])
        c["axis_dists"].setdefault(record.get("axis") or "?", set()).add(record["dist"])
        if where.startswith("load:"):
            c["blocked_load_dists"].add(record["dist"])
        else:
            c["files"] += 1
            c["assertions"] += assertions
        # For a family-level cluster the payload is the thing a reader needs to
        # see the spread of (which trait, which typename, which parent). Counted
        # in distributions like everything else here: one distribution with forty
        # modules is one bug, and a per-site count says otherwise.
        c["variant_dists"].setdefault(variant, set()).add(record["dist"])
        if len(c["sites"]) < 6:
            c["sites"].append({
                "dist": record["dist"],
                "version": record.get("version"),
                "where": where,
                "message": message[:300],
            })
    clusters = []
    for c in out.values():
        c["n_dists"] = len(c["dists"])
        c["dists"] = sorted(c["dists"])
        c["blocked_load_dists"] = sorted(c["blocked_load_dists"])
        c["axes"] = {a: len(d) for a, d in sorted(c.pop("axis_dists").items())}
        c["variants"] = sorted(((v, len(d)) for v, d in c.pop("variant_dists").items()),
                               key=lambda vd: (-vd[1], vd[0]))[:8]
        clusters.append(c)
    clusters.sort(key=lambda c: (-c["n_dists"], -c["assertions"], c["signature"]))
    return clusters


# --- reporting ---------------------------------------------------------------


def print_table(clusters, *, min_dists, limit):
    """`clusters` is every cluster; min_dists/limit only affect what is printed.

    The family rollup is deliberately computed over ALL of them: the long tail is
    where most of the corpus's distributions are, and a rollup that silently
    inherited the table's cutoff would hide that.
    """
    shown = [c for c in clusters if c["n_dists"] >= min_dists][:limit]
    print(f"{'id':8}  {'dists':>5} {'files':>5} {'asserts':>7}  {'family':28} signature")
    for c in shown:
        print(f"{c['id']:8}  {c['n_dists']:5} {c['files']:5} {c['assertions']:7}  "
              f"{c['family']:28} {c['signature'][:64]}")
    shown_ids = {id(c) for c in shown}
    rest = [c for c in clusters if id(c) not in shown_ids]
    print(f"\n{len(shown)} of {len(clusters)} clusters shown; the other {len(rest)} cover "
          f"{len({d for c in rest for d in c['dists']})} distributions and "
          f"{sum(c['assertions'] for c in rest)} rakudo assertions")
    fam = collections.defaultdict(set)
    per_dist = collections.Counter()
    for c in clusters:
        fam[c["family"]].update(c["dists"])
        if c["signature"].endswith("]") and c["signature"].startswith(c["family"] + " ["):
            per_dist[c["family"]] += 1
    print("\nby family (distributions, deduped across signatures):")
    for f, d in sorted(fam.items(), key=lambda kv: -len(kv[1])):
        note = (f"  -- {per_dist[f]} per-distribution clusters, no shared root cause"
                if per_dist[f] else "")
        print(f"  {len(d):4}  {f}{note}")


BLOB = "https://github.com/tokuhirom/mutsu/blob/main"


def issue_body(c, *, summary=None):
    """A self-contained `todo:*` issue body for one cluster."""
    head = [
        "Measured by the ecosystem parity ledger "
        "([#7785](https://github.com/tokuhirom/mutsu/issues/7785) P5, "
        f"[ADR-0085]({BLOB}/docs/adr/0085-ecosystem-testsuite-parity-measurement.md)). "
        "One root cause, grouped across the corpus by `scripts/ecosystem-tickets.py`.",
        "",
        f"`eco-cluster: {c['id']}`  ·  family `{c['family']}`",
        "",
        "## Impact",
        "",
        "| | |",
        "|---|---|",
        f"| distributions affected | **{c['n_dists']}** |",
        f"| of which cannot even `use` their own modules (`blocked_load`) | {len(c['blocked_load_dists'])} |",
        f"| failing test files | {c['files']} |",
        f"| rakudo assertions in those files (what `assertion_parity` gains) | {c['assertions']} |",
        f"| axis | {', '.join(f'{k} {v}' for k, v in sorted(c['axes'].items()))} |",
        "",
        "## The failure",
        "",
        f"Signature: `{c['signature']}`",
        "",
    ]
    if len(c["variants"]) > 1:
        head += ["What varies between them (count = distributions):", ""]
        head += [f"- `{v}` ×{n}" for v, n in c["variants"]]
        head += [""]
    head += ["## Sites", "",
             "Each of these is a file rakudo passes cleanly, or a module rakudo loads, "
             "in the same sandbox with the same `-I` list.", ""]
    for s in c["sites"]:
        head.append(f"- **{s['dist']}** {s['version'] or ''} — `{s['where']}`  \n  `{s['message']}`")
    listed, rest = c["dists"][:30], max(0, c["n_dists"] - 30)
    head += ["", f"<details><summary>Affected distributions ({c['n_dists']})</summary>", "",
             ", ".join(f"`{d}`" for d in listed)
             + (f", and {rest} more -- `scripts/ecosystem-tickets.py --issue {c['id']}` "
                "lists them from the ledger" if rest else ""),
             "", "</details>", "",
             "## Reproducing", "",
             "```sh",
             f"scripts/ecosystem-sweep.py --only {c['sites'][0]['dist']}   # re-measures one record",
             f"scripts/ecosystem-tickets.py --issue {c['id']}    # this report, from the ledger",
             "```",
             "",
             "The per-distribution workflow (check the tarball out, run both sides file by "
             f"file, minimise) is the [`ecosystem-dist-fix`]({BLOB}/.agents/skills/"
             "ecosystem-dist-fix/SKILL.md) skill. **Add a `t/` regression test for the "
             "minimised case**, not for the distribution.",
             ""]
    if summary:
        head += ["## Notes", "", summary, ""]
    return "\n".join(head)


# --- self-test ---------------------------------------------------------------

def self_test() -> int:
    cases = [
        ("Can't use unknown trait 'is' -> 'json-skip-null' in an attribute declaration.",
         "unknown-attribute-trait", "unknown-attribute-trait"),
        ("Can't use unknown trait 'is' -> 'xml-element' in an attribute declaration.",
         "unknown-attribute-trait", "unknown-attribute-trait"),
        ("No such method 'AST' for invocant of type 'Str'",
         "no-such-method", "No such method 'AST' on Str"),
        ("Runtime error: Failed to parse module 'Red': Unexpected block in infix position "
         "(missing statement control word before the expression?)",
         "other", "Unexpected block in infix position (missing statement control word before "
         "the expression?)"),
        ("expected statement: expected expected statement: expected expression statement or ')'",
         "parse-error-expectation-dump", None),
        ("Confused. expected statement: expected use statement or import statement or ...",
         "parse-error-expectation-dump", None),
        ("Unsupported nqp:: op: nqp::p6bindattrinvres", "nqp-op",
         "Unsupported nqp::p6bindattrinvres"),
        # The harness's location suffix must not reach the signature, or every
        # record becomes its own cluster.
        ("Unsupported nqp:: op: nqp::hash [at dist/lib/A.rakumod:12]", "nqp-op",
         "Unsupported nqp::hash"),
        ("Invalid typename 'IndRef' in parameter declaration.", "invalid-typename", None),
        ("thread '<unnamed>' (12) has overflowed its stack", "stack-overflow", None),
        ("SWEEP-TIMEOUT", "timeout", None),
    ]
    # An unnamed failing assertion must NOT get a shared signature -- see
    # classify(). This is the one case where `None` is the required answer.
    if classify("not ok 7 -")[:2] != ("wrong-answer-unnamed-assertion", None):
        print(f"FAIL unnamed assertion clustered: {classify('not ok 7 -')}")
        return 1
    # A family-level cluster's payload must survive: it is the only thing that
    # distinguishes its members, and normalising it away made every trait in the
    # corpus read as the same one.
    variants = {classify(f"Can't use unknown trait 'is' -> '{t}' in an attribute "
                         "declaration.")[2] for t in ("json-skip-null", "xml-element")}
    if len(variants) != 2:
        print(f"FAIL family payload collapsed: {variants}")
        return 1
    fails = 0
    for message, want_family, want_sig in cases:
        family, sig, _variant = classify(message)
        if family != want_family or (want_sig is not None and sig != want_sig):
            fails += 1
            print(f"FAIL {message[:60]!r}\n  got  ({family}, {sig})\n  want ({want_family}, {want_sig})")
    # A parse failure must not cluster by the module it was compiling: two
    # distributions hitting one parser gap have to land in the same cluster.
    a = classify("Failed to parse module 'A::B': Cannot interpolate attribute in a regex")[:2]
    b = classify("Failed to parse module 'C::D': Cannot interpolate attribute in a regex")[:2]
    if a != b:
        fails += 1
        print(f"FAIL module name leaked into the signature: {a} != {b}")
    # Noise must be recognised, and must not be silently reclassified as a bug.
    if noise_class("Failed to resolve 'x': failed to lookup address information") != "network":
        fails += 1
        print("FAIL network noise not classified")
    if noise_class("No such method 'AST' for invocant of type 'Str'") is not None:
        fails += 1
        print("FAIL a real failure was classified as noise")
    # The id is a pure function of the signature (stable across sweeps).
    ids = {c: hashlib.sha256(c.encode()).hexdigest()[:8] for c in ("a", "b")}
    if len(set(ids.values())) != 2:
        fails += 1
    print(f"ecosystem-tickets self-test: {len(cases) + 4} checks, {fails} failed")
    return 1 if fails else 0


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--limit", type=int, default=30, help="rows in the table (default 30)")
    ap.add_argument("--all", action="store_true", help="every cluster, no row limit")
    ap.add_argument("--min-dists", type=int, default=2,
                    help="drop clusters affecting fewer distributions (default 2)")
    ap.add_argument("--family", help="only this family")
    ap.add_argument("--class", dest="cls", choices=("actionable", "all"), default="actionable",
                    help="`all` also shows sites the sweep's own host caused")
    ap.add_argument("--issue", metavar="ID", help="print a ready-to-file issue body for a cluster")
    ap.add_argument("--note", help="with --issue: a Notes paragraph (what you already know)")
    ap.add_argument("--json", metavar="OUT", help="write every cluster as JSON")
    ap.add_argument("--self-test", action="store_true")
    args = ap.parse_args()

    # `| head` on a 985-row table would otherwise end in a BrokenPipeError trace.
    try:
        import signal
        signal.signal(signal.SIGPIPE, signal.SIG_DFL)
    except (ImportError, AttributeError, ValueError):
        pass

    if args.self_test:
        raise SystemExit(self_test())

    records = eco.load_records(DISTS_DIR)
    clusters = cluster(records, want_class=args.cls)
    if args.family:
        clusters = [c for c in clusters if c["family"] == args.family]

    if args.issue:
        for c in clusters:
            if c["id"] == args.issue:
                print(issue_body(c, summary=args.note))
                return
        raise SystemExit(f"no cluster with id {args.issue} "
                         "(ids come from a run with the same --class/--min-dists)")

    if args.json:
        with open(args.json, "w", encoding="utf-8") as fh:
            json.dump({"clusters": clusters, "records": len(records)}, fh, indent=2)
        print(f"wrote {len(clusters)} clusters -> {args.json}")

    print_table(clusters, min_dists=args.min_dists,
                limit=len(clusters) if args.all else args.limit)


if __name__ == "__main__":
    main()
