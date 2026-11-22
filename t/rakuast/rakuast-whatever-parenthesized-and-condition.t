use experimental :rakuast;
use Test;

# Which `*` leaves `.AST` renders as a `Term::Whatever` value and which as a
# priming `WhateverCode::Argument` (#18688), measured on rakudo 2026.11: a
# parenthesized `*` is a value except as a method call's target, and so is a
# bare `*` condition.

plan 12;

sub leaves($src) {
    my @l;
    sub w($n) {
        @l.push($n.^name.subst('RakuAST::', '')) if $n.^name ~~ /Whatever/;
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(',')
}

is leaves(Q|say (*)|), 'Term::Whatever', 'a parenthesized argument';
is leaves(Q|say (((*)))|), 'Term::Whatever', 'nested parentheses';
is leaves(Q|(*).abs|), 'WhateverCode::Argument', 'a method target primes through one layer';
is leaves(Q|((*)).abs|), 'Term::Whatever', 'two layers freeze it';
is leaves(Q|my $x = (*)|), 'Term::Whatever', 'a parenthesized initializer';
is leaves(Q|given 1 { when * { } }|), 'Term::Whatever', 'a when condition';
is leaves(Q|given 1 { when (*) { } }|), 'Term::Whatever', 'a parenthesized when condition';
is leaves(Q|if * { }|), 'Term::Whatever', 'an if condition';
is leaves(Q|say (* + 1)|), 'WhateverCode::Argument', 'an operand still primes';
is leaves(Q|say ((*) + 1)|), 'Term::Whatever', 'a parenthesized operand is a value';
is leaves(Q|given 1 { when * > 0 { } }|), 'WhateverCode::Argument', 'a compound condition primes';
is leaves(Q|1 ~~ *|), 'WhateverCode::Argument', 'a smartmatch right side primes';
