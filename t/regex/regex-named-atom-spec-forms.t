use Test;

# A `<subrule>` atom's lookup spec — silent or not, aliased or not, with or
# without arguments — is a pure function of the atom text, and the matcher used
# to re-derive it per call through a process-wide `text -> spec` map. It is
# memoized on the atom node now (`NamedAtom::spec`), so the derivation happens
# once per written atom instead of once per call
# (https://github.com/tokuhirom/mutsu/issues/7576).
#
# The spec is what decides the capture NAME and visibility of every one of these
# forms, so this file pins one of each: a wrong or stale spec on a node shows up
# directly as a capture filed under the wrong key (or not at all). Verified
# against rakudo 2026.07.

plan 10;

grammar G {
    token TOP     { <plain> ' ' <.silent> ' ' <al=word> ' ' <dot=.word> }
    token plain   { \w+ }
    token silent  { \w+ }
    token word    { \w+ }
}

my $m = G.parse('aa bb cc dd');
ok $m.defined, 'the whole mixture of subrule forms parses';
is $m<plain>.Str, 'aa', 'a plain <subrule> captures under its own name';
nok ($m<silent>:exists), 'a silent <.subrule> captures nothing';
is $m<al>.Str, 'cc', 'a non-suppressing alias <al=word> captures under the alias';
is $m<word>.Str, 'cc', '...and under the original rule name too';
is $m<dot>.Str, 'dd', 'a dot alias <dot=.word> captures under the alias only';
nok ($m<TOP>:exists), 'no stray capture leaked in';

grammar Args {
    token TOP     { <rep(3)> }
    token rep($n) { \w ** {$n} }
}
is Args.parse('xyz')<rep>.Str, 'xyz', 'a parameterized <rule(args)> call keeps its own name';

grammar Q {
    token TOP  { <item>+ % ',' }
    token item { \w+ }
}
is Q.parse('a,b,c')<item>».Str.join('|'), 'a|b|c',
    'a quantified subrule files every iteration under the one name';

# The same atom text in two grammars resolves to each grammar's own rule: the
# spec is shared per text, the RESOLUTION is not.
grammar A { token TOP { <v> }; token v { 'a' } }
grammar B { token TOP { <v> }; token v { 'b' } }
ok A.parse('a').defined && B.parse('b').defined && !A.parse('b').defined,
    'one atom text, two grammars, each resolving to its own rule';
