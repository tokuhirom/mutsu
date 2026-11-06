use v6;
use lib 't/lib-bare-file-export-multi';
use Test;

# A bare file module's `is export` multi candidates of a core operator are not
# a `proto`: importing them must leave the core `infix:<*>` candidates in the
# running, so an unrelated `multi infix:<*>` declared in a role body whose own
# body multiplies the same operands reaches the core candidate instead of
# recursing into itself. Found with CSS::Properties (CSS::Units + Color).
# Identical under raku.

role Unit[\units] {
    method scale(Numeric $s) { self * $s }
    multi sub infix:<*>(Unit $v, Numeric $n) {
        ($v * $n) but Unit[units];
    }
}
role Unit {
    use BareOps;
}

my $m = 10 but Unit['mm'];
is $m.scale(1.5), 15, 'self * $s reaches the core candidate';
is $m.scale(2), 20, 'Int operand too';

done-testing;
