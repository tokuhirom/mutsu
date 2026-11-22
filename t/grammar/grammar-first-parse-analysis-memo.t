use Test;

# The analyses a grammar's first parse runs once per generation (#17640): the
# proto-variant index over the registered rules, the per-atom ASCII sets of a
# compiled rule, the "are this rule's call edges stable" verdict and the
# subrule argument-list parse. Each is memoized; these pin that the answers
# are the same as before and follow a redefinition.

plan 9;

grammar Outer::G {
    proto token item {*}
    token item:sym<num>   { \d+ }
    token item:sym«word»  { <[a..z]>+ }
    token item:punct      { <[,;]> }
    token TOP { <item>+ % ' ' }
}
is Outer::G.parse('12 ab ;').<item>.map(~*).join('|'), '12|ab|;',
    'proto variants of a package-qualified grammar, all three spellings';

# A second grammar whose rule name shares a prefix with another's.
grammar Outer::G2 is Outer::G {
    token item:sym<upper> { <[A..Z]>+ }
}
is Outer::G2.parse('12 AB x').<item>.map(~*).join('|'), '12|AB|x',
    'a subclass adds a variant to the inherited proto';
nok Outer::G.parse('12 AB'), '... which the parent grammar does not have';

# A grammar defined after the first parse (a new generation) adds variants.
use MONKEY-SEE-NO-EVAL;
grammar Late { proto token t {*}; token t:sym<a> { a }; token TOP { <t> } }
ok Late.parse('a'), 'first use';
my \Later = EVAL q[grammar Later is Late { token t:sym<b> { b } }; Later];
ok Later.parse('b'), 'a variant of a grammar defined later is found';
nok Late.parse('b'), '... and the earlier grammar still lacks it';

# ASCII sets are probed per atom on first use.
grammar Atoms { token TOP { <[a..c]>+ \d ['x' | 'y'] <-[q]> } }
is ~Atoms.parse('abc3yz'), 'abc3yz', 'atoms of one program matched in turn';

# Subrule argument lists, with and without a sequence operator.
grammar Args {
    token TOP { <one(1, 2)> <seq(1 ... 3)> }
    token one($a, $b) { { $*got ~= "$a$b|" } . }
    token seq($s)     { { $*got ~= $s.join(',') } . }
}
my $*got = '';
ok Args.parse('xy'), 'calls with arguments parse';
is $*got, '12|1,2,3', 'positional and sequence arguments arrive';
