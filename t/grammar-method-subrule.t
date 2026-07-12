use v6;
use Test;

plan 5;

# A subrule that names a plain grammar method calls it; an exception thrown
# inside the method propagates out of the parse instead of being swallowed.
grammar WithMethod {
    rule TOP { 'lorem' | <.panic> }
    method panic { die "The sky is falling!"; }
}

dies-ok { WithMethod.parse('unrelated') },
    'die() from a method-subrule propagates out of the parse';

try { WithMethod.parse('unrelated') };
ok ~$! ~~ /'The sky is falling!'/, 'and the exception message is preserved';

# An embedded { ... } block in a grammar rule closes over the outer lexical
# scope where the grammar was defined.
my $x = 0;
grammar WithOuterLex {
    regex TOP { x { $x = 42 } }
}
WithOuterLex.parse('xxx');
is $x, 42, 'regex code block in a grammar writes an outer lexical';

# The write reaches the outer lexical even from inside a sub scope.
sub in-sub {
    my $y = 0;
    grammar G { regex TOP { x { $y = 7 } } }
    G.parse('x');
    $y;
}
is in-sub(), 7, 'outer lexical write works from a sub-local scope';

# A duplicate token/regex declaration in a package is a redeclaration error
# phrased the Rakudo way ("already has a regex").
try { EVAL 'grammar Dup { token a { ... }; token a { ... } }' };
ok ~$! ~~ /:i 'already has a regex' /, 'duplicate token errors sanely';
