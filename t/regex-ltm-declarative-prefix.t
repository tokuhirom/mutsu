use v6;
use Test;

plan 11;

# LTM candidate selection measures how far each candidate *declaratively* matches.
# A code atom (`{ … }`, `<?{ … }>`, `<!{ … }>`) terminates the declarative prefix,
# so measuring must never execute one — otherwise every measurement duplicates
# that candidate's side effects. See docs/adr/0009.
#
# An assertion at a single matched position runs exactly once, as in raku. Before
# the LTM fix mutsu ran it 4 times: two of the extra runs were candidate-length
# measurements (one per LTM measurement the engine made), the third was a replay
# in the parent interpreter for side effects, which part B removed by running the
# assertion inline on the real interpreter in the first place.

our %n;

grammar Inline {
    token TOP { (\w) <?{ %n<inline>++; True }> }
}
grammar ViaSubrule {
    token TOP { <item> }
    token item { (\w) <?{ %n<sub>++; True }> }
}
grammar Deep {
    token TOP { <a> }
    token a { <b> }
    token b { <c> }
    token c { (\w) <?{ %n<deep>++; True }> }
}

%n = ();
ok Inline.parse("a").defined, 'assertion directly in the start rule: parses';
is %n<inline>, 1, 'the assertion runs exactly once — no measurement executes it';

# One subrule down: the start rule's measurement must not descend into the
# subrule and execute the assertion nested there either.
%n = ();
ok ViaSubrule.parse("a").defined, 'assertion one subrule down: parses';
is %n<sub>, 1, 'measuring the start rule does not execute a subrule assertion';

# Nesting depth must not multiply the count.
%n = ();
ok Deep.parse("a").defined, 'assertion three subrules down: parses';
is %n<deep>, 1, 'assertion run count is independent of grammar nesting depth';

# A candidate whose declarative prefix is empty (the code atom comes first) must
# still be selectable: the measurement stops immediately and so proves nothing
# about whether the candidate matches — it has to survive and let the real match
# decide.
grammar CodeFirst {
    token d   { <?{ True }> <[0..9]> }
    token TOP { <+d>+ }
}
ok  CodeFirst.parse('42').defined, 'candidate with a leading code atom still matches';
nok CodeFirst.parse('4a').defined, 'and still rejects a non-match';

# Proto LTM ordering is unaffected for assertion-free candidates (the common case):
# the longest declarative prefix still wins regardless of declaration order.
grammar Proto {
    proto token t {*}
    token t:sym<short> { 'ab' }
    token t:sym<long>  { 'abcd' }
    token TOP { <t> }
}
is ~Proto.parse('abcd'), 'abcd', 'longest declarative prefix still wins LTM';

# ADR-0022 Slice 1: the declarative-prefix measurement now sees through more
# atom kinds than a bare code block (ADR-0009's original scope) — a
# backreference, and the `ws` rule, both TERMINATE the measurement instead of
# running through it as an ordinary (consuming) atom. Two proto candidates
# below are each real, independently-viable matches; which one measures
# longer flips once the terminator is honored, so the WINNING candidate
# (and hence whether the surrounding `.parse`, which requires full
# consumption, succeeds at all) changes. Both expectations are verified
# against `raku` directly (2026-08-10) — `raku` also reports these as
# non-matches, for the same reason: its NFA has no method for `$0` or `ws`,
# so they end the branch's fate length right where they are reached.
#
# The proto must be the grammar's OWN start rule (`TOP`), not referenced via
# `<name>` from a separate `TOP` — a `<name>` subrule reference inside
# another pattern's body resolves through the regex engine's own named-atom
# proto loop (`regex_match_atom.rs`), which ranks by longest ACTUAL match and
# is untouched by this ADR (that is Slice 3 territory too). Only
# `Grammar.parse`'s start-rule candidate selection
# (`dispatch.rs::eval_token_call_values_at`) already ranks by declarative
# prefix (ADR-0009) and is what this slice's extended atom-mode table changes
# the measurement for.
grammar BackrefProto {
    # Old (pre-Slice-1) measurement ran the backref for real during
    # candidate ranking, measuring the full 3-char "aaZ"; the correct
    # (post-Slice-1) measurement terminates right after the capture group,
    # at 1 char — shorter than sym<pair>'s fully-declarative 2 chars below.
    proto token TOP {*}
    token TOP:sym<backref> { (\w) $0 'Z' }
    token TOP:sym<pair>    { \w \w }
}
nok BackrefProto.parse('aaZ').defined,
    'backref terminates the LTM prefix, so the shorter fully-declarative candidate now ranks first and the parse (needing full consumption) fails';

grammar WsProto {
    # Old measurement ran <.ws> for real (consuming the space), measuring
    # the full 4-char "ab Z"; the correct measurement terminates right after
    # `\w+`, at 2 chars — shorter than sym<lit>'s fully-declarative 3 chars.
    proto token TOP {*}
    token TOP:sym<ws>  { \w+ <.ws> 'Z' }
    token TOP:sym<lit> { 'ab' ' ' }
}
nok WsProto.parse('ab Z').defined,
    'the ws rule terminates the LTM prefix, so the shorter fully-declarative candidate now ranks first and the parse (needing full consumption) fails';
