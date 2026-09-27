use Test;

plan 12;

# #9675: Rakudo decides list-vs-singular for a capture NAME statically, from
# the whole regex (QRegex's `capnames`: quantified, or bound more than once in
# the same sequence, means the name is list-valued everywhere in the pattern)
# -- so a name that is list-valued in ONE alternation branch renders as an
# empty LIST, not `Nil`, even when a DIFFERENT branch is the one that actually
# matched. mutsu used to seed the empty-list default only at the zero-match
# point of each quantifier, which never runs for a branch the cursor never
# entered at all.

grammar G1 { token TOP { <e>* }; token e { \w } }
is G1.parse('')<e>.raku, '[]', 'zero-iteration <e>* is an empty list (sanity)';

grammar G2 { token TOP { 'x' | <e>+ }; token e { \w } }
is G2.parse('x')<e>.raku, '[]',
    'quantified subrule name in the untaken branch of | is an empty list';

grammar G3 { token TOP { [ 'x' | <e>+ ] } ; token e { \w } }
is G3.parse('x')<e>.raku, '[]',
    'same, with the alternation wrapped in a non-capturing group';

grammar G4 { token TOP { [<e>+]? }; token e { \w } }
is G4.parse('')<e>.raku, '[]',
    'quantified subrule name under a zero-matched ? stays an empty list';

grammar G5 { token TOP { 'x' | <e> <e> }; token e { \w } }
is G5.parse('x')<e>.raku, '[]',
    'a name bound twice (unquantified) in the untaken branch is still a list';

grammar G6 { token TOP { 'x' | <e> }; token e { \w } }
is G6.parse('x')<e>.raku, 'Nil',
    'a name bound at most once anywhere stays absent (Nil), not a list';

is ('x' ~~ / 'x' | $<n>=[\w]+ /)<n>.raku, 'Nil',
    'an explicit alias on a quantified atom is singular, not list, in the untaken branch';

is ('x' ~~ / 'x' | (\w)+ /)[0].raku, '[]',
    'a quantified positional capture group in the untaken branch is an empty list';

# The ANTLR4::Grammar shape this was found in (#9491): an action reads a
# never-taken branch's quantified subrule capture as a list, not `Nil`.
grammar G7 {
    rule TOP { <lexerElement>+ | '' }
    token lexerElement { \w }
}
my @child = G7.parse('')<lexerElement>>>.ast;
is @child.elems, 0, 'an action-style .ast map over the untaken branch works on []';

# ABC 0.6.13 exposed the complementary case: a name that is list-valued in a
# later alternation branch must remain a List when the earlier branch matches
# and supplies the one capture.
grammar G8 {
    token TOP { <e> | [ '[' <e>+ ']' ] }
    token e { \w }
}
my $taken = G8.parse('a');
is $taken<e>.WHAT.raku, 'Array', 'a selected branch keeps a statically list-valued name';
is $taken<e>.elems, 1, 'the selected branch contributes one list entry';
is $taken<e>[0].Str, 'a', 'the selected branch list entry has the captured text';
