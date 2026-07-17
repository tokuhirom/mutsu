use v6;
use Test;

plan 9;

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
