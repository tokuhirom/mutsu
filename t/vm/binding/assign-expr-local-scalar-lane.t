use nqp;
use Test;

# An assignment used as an expression (`($i = $i - 1)` inside `nqp::stmts`,
# `if ($x = f())`) stores like the statement form and yields the stored value,
# and still refuses a read-only target.

plan 8;

sub countdown() {
    my int $i = 5;
    my int $n = 0;
    nqp::while(nqp::isgt_i($i, 0), nqp::stmts(($i = nqp::sub_i($i, 1)), ($n = nqp::add_i($n, 1))));
    "$i $n"
}
is countdown(), '0 5', 'expression assignments in an nqp loop body';

my $x;
is ($x = 42), 42, 'the assignment yields the value';
is $x, 42, 'and stores it';

my $y = 1;
my $z = ($y = 2) + 1;
is "$y $z", '2 3', 'the value takes part in the enclosing expression';

sub ro($p) { ($p = 5) }
throws-like { ro(1) }, X::AdHoc, 'assigning to a read-only parameter dies';

my Int $t = 1;
throws-like { ($t = 'str') }, X::TypeCheck::Assignment, 'a typed variable still type-checks';

my $item = ($x = [1, 2]);
is $item.raku, '$[1, 2]', 'an array assigned to a scalar is itemized';

my int8 $w;
is ($w = 300), 44, 'a narrow native int wraps';
