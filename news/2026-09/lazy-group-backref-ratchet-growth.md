# A backreference after a lazy group can grow past a rejected end again under ratchet

The here-doc idiom (`<<EOF ... EOF`, where the closing marker must equal the
opening one) failed to match at all in a `grammar`/`token`/`rule` context
(#8624):

```raku
grammar G {
    rule TOP { <here-doc> }
    token start-here-doc { '<<' }
    token name { \S+ }
    rule here-doc {
        <start-here-doc><name>
        $<here-doc-value>=[.*?]
        $<name>
    }
}
G.parse("<<EOF\nbar\nEOF\n");   # raku: MATCHED   mutsu (before): NO MATCH
```

## Root cause

`drive_subpattern_candidates` (ADR-0073's demand-driven candidate producer
for `Group`/`CaptureGroup`/`CaptureIsolatedGroup` atoms) drives a group's
inner pattern through a `MatchSink::Cont` and, whenever the enclosing
token/rule's ratchet flag was set, stopped asking for a longer end the
moment the continuation rejected the first one it offered. That is correct
for the group's own alternation/greedy-quantifier choices — ratchet forbids
reconsidering an already-matched one, confirmed against raku with
`token t { [ab|a] 'bc' }` on `"abc"` (NO MATCH: `ab` matches, `'bc'` then has
too little left, and ratchet forbids falling back to `a`).

But it was too strong for a *frugal* quantifier nested inside a
**non-capturing** `[...]` group: `walk_quant_chain` already lets a directly
written `\S+?`/`.*?` keep growing shortest-first even under ratchet (raku
grows a frugal quantifier regardless of ratchet — ratchet only changes
*greedy* behavior, commit-to-longest-no-backtrack). A `[...]` group is
transparent grouping, not a capture boundary, so the same growth has to be
allowed to happen *through* it — `[.*?]` in the same position as an unwrapped
`.*?` must behave identically, and before this fix it did not.

A **capturing** `(...)` group is different: once it has produced a captured
value, ratchet does commit to it, confirmed against raku with
`token t { (\S+) $<mid>=(.*?) $0 }` on `"foo XXX foo"` (NO MATCH, unlike the
non-capturing `[...]` spelling of the same pattern, which matches). So the
fix only relaxes the early-stop for the `Merge` (non-capturing) shape;
`Capture` and `Isolated` keep committing to the first candidate under
ratchet exactly as before. The relaxed `Merge` case relies on the inner
pattern's own per-token ratchet/frugal rules (already correct) to stop
asking for more once it has genuinely run out of admissible ends — nothing
about alternation-branch-commit or greedy-quantifier-no-backtrack needed to
change.

Regression coverage: `t/regex/syntax/regex-ratchet-frugal-group-backref.t`,
which pins the here-doc idiom itself, the minimal token-level shape with and
without the grammar, and every one of the raku-verified invariants above
(greedy `[.*]` still refuses to backtrack, alternation inside `[...]` still
can't reconsider a matched branch, a capturing group still commits to its
first candidate, and the pre-existing `:r` capturing-group scan-position
retry behaviour is unaffected).
