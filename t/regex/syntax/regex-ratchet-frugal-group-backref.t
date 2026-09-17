use Test;

# #8624: a backreference (`$<name>` / `$0`) after a lazy-quantified
# non-capturing group (`[...]`) inside a ratcheted `token`/`rule` failed to
# match at all -- the whole rule reported no match instead of growing the
# frugal group to satisfy the backreference.
#
# Root cause: `drive_subpattern_candidates` (ADR-0073's streamed group-atom
# candidate producer) stopped asking a `[...]` group's inner walk for a
# longer end the moment the FIRST end it offered was rejected by the
# continuation, whenever the enclosing token/rule's ratchet flag was set on
# the group's own wrapping token. That is correct for the group's own
# alternation/greedy-quantifier choices (ratchet forbids reconsidering an
# already-matched one), but wrong for a FRUGAL quantifier nested inside the
# group: raku keeps growing a frugal quantifier even under ratchet
# (`walk_quant_chain` already implements that for a directly-quantified
# atom), and a non-capturing group is transparent grouping, not a capture
# boundary, so the same growth must be allowed to happen through it.
#
# A CAPTURING group (`(...)`) is different: once it has produced a captured
# value, ratchet commits to it (verified against raku below), so only the
# `[...]` (Merge) shape gets the relaxed treatment.
#
# Every expectation here was verified against real `raku`.

plan 12;

# The exact grammar shape from the issue: a here-doc idiom, `<<EOF ... EOF`,
# where the closing marker must equal the opening one.
{
    grammar HereDoc {
        rule TOP { <here-doc> }
        token start-here-doc { '<<' }
        token name { \S+ }
        rule here-doc {
            <start-here-doc><name>
            $<here-doc-value>=[.*?]
            $<name>
        }
    }
    my $r = HereDoc.parse("<<EOF\nbar\nEOF\n");
    ok $r, 'here-doc idiom: backref to a subrule capture after a lazy group matches';
    is $r<here-doc><here-doc-value>.Str, "bar",
        'and the lazy group captured exactly the text between the markers';
}

# Minimal token-level repro, no grammar: backref to an inline capture after a
# lazy non-capturing group grows correctly under ratchet.
my token tok1 { (\S+) \s $<mid>=[.*?] \s $0 }
ok "foo XXX foo" ~~ &tok1,
    'token: backref after [.*?] grows past a rejected end under ratchet';

# Backreference to a SUBRULE capture (not an inline one) after the same lazy
# group, inside a grammar -- the shape the issue was actually found through.
{
    grammar G2 {
        rule TOP { <name> $<mid>=[.*?] <.ws> $<name> }
        token name { \S+ }
    }
    ok G2.parse("foo XXX foo"), 'grammar: backref to a subrule capture grows past a lazy group';
}

# The unwrapped quantifier (no `[...]`) already worked; it must keep working.
my token tok3 { (\S+) \s .*? \s $0 }
ok "foo XXX foo" ~~ &tok3,
    'token: backref after an unwrapped .*? still matches (control)';

# A non-ratcheted `regex`/plain `m//` already worked; it must keep working.
ok "foo XXX foo" ~~ / (\S+) \s $<mid>=[.*?] \s $0 /,
    'plain regex (non-ratchet): backref after [.*?] still matches (control)';

# A GREEDY (non-frugal) group must still NOT backtrack under ratchet -- this
# is the invariant the fix must not weaken.
my token tok6 { (\S+) \s $<mid>=[.*] \s $0 }
nok "foo XXX foo" ~~ &tok6,
    'token: a GREEDY [.*] group still does not backtrack under ratchet';

# An alternation's branch choice must still be forbidden from reconsideration
# under ratchet, even inside a `[...]` group (this is a real backtrack, not a
# frugal quantifier growing): LTM ranks 'ab' ahead of 'a' (longer literal), it
# matches, but the continuation 'bc' then has too little left -- and ratchet
# forbids falling back to the 'a' branch, even though that WOULD continue.
my token tok7 { [ab|a] 'bc' }
nok "abc" ~~ &tok7,
    'token: alternation inside [...] still cannot backtrack under ratchet';
# Same alternation, but the LTM-preferred branch's own continuation succeeds
# immediately -- no backtracking needed, so this must keep matching.
my token tok8 { [ab|a] 'c' }
ok "abc" ~~ &tok8,
    'token: ... and the LTM-preferred branch matches when it needs no backtrack';

# A CAPTURING group is different from `[...]`: once `(...)` has produced a
# value, ratchet commits to it, so growing a frugal quantifier inside a
# capturing group must NOT be allowed to retry a longer capture.
my token tok9 { (\S+) \s $<mid>=(.*?) \s $0 }
nok "foo XXX foo" ~~ &tok9,
    'token: a CAPTURING (.*?) group does not grow past a rejected end under ratchet';

# The `:r` capturing-group-retries-at-each-start-position behaviour (a
# different mechanism -- scanning start positions, not backtracking within
# one) must be unaffected.
{
    my $c = 0;
    "aaac" ~~ / :r ( \w* { $c++ } ) c /;
    is $c, 5, ':r capturing group still retries at each scan start position';
}

# A plain (non-frugal, non-alternation, non-capturing) group with a
# fixed-width atom inside is unaffected either way.
my token tok10 { [ 'a' ] 'b' }
ok "ab" ~~ &tok10,
    'token: a plain fixed-width [...] group is unaffected';
