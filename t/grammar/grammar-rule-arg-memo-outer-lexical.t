use Test;

# A parameterized rule (`<rule($arg)>`) is resolved by binding the arguments in
# a scratch interpreter, evaluating the body and rendering the pattern; the
# result is memoized on (package, rule name, arguments). An entry may only be
# stored when that chain read nothing outside the key — so a body that splices
# an OUTER lexical must not be served from the memo after that lexical changes.
#
# Verified against rakudo 2026.07.
# https://github.com/tokuhirom/mutsu/issues/7576

plan 7;

my $tail = 'a';
grammar Spliced {
    token TOP(Int $n) { 'x' ** { $n } $tail }
}

ok Spliced.parse('xxa', :args(\(2))), 'outer lexical spliced into a parameterized rule';
$tail = 'b';
ok Spliced.parse('xxb', :args(\(2))), 'the same rule and arguments see the NEW value';
nok Spliced.parse('xxa', :args(\(2))), 'and no longer the old one';

# A rule whose body reads nothing but its own parameters IS memoizable, and
# repeated resolutions must keep producing the same match — including the
# `$<name>=[...]` capture forms, which the syntactic predicate this replaced
# rejected outright.
grammar Rows {
    token TOP($pad) { <row($pad)>+ % "\n" }
    token row($pad) { $pad $<val> = [ \w+ ] }
}

my $m = Rows.parse("  ab\n  cd\n  ef", :args(\('  ')));
ok $m, 'a parameterized rule with a named capture parses';
is $m<row>.elems, 3, 'every repetition matched';
is $m<row>[0]<val>, 'ab', 'first capture';
is $m<row>[2]<val>, 'ef', 'last capture';
