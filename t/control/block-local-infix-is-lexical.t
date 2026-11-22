use MONKEY-SEE-NO-EVAL;
use Test;

# A plain `sub infix:<op>` declared in a block is a declaration of that
# block's lexical scope: every use of the operator written inside the block
# calls it, and a routine written outside the block keeps the built-in
# operator even when the block calls it (#17771).

plan 17;

sub third { 3 ** -1 }
sub outside-amp { &infix:<**>(2, 3) }
sub outside-reduce { [**] 2, 3 }

{
    sub infix:<**>(\a, \b) { 42 }
    is 3 ** -1, 42, 'the block-local operator answers a use in the block';
    is-approx third(), 1/3, 'a routine written outside the block keeps the built-in';
    is outside-amp(), 8, '... also by its &infix:<**> name';
    is outside-reduce(), 8, '... and in a reduction';
    is ([**] 2, 3), 42, 'a reduction in the block uses the block-local operator';
    is ([\**] 2, 3).List, (3, 42), 'so does a triangle reduction';
    is ((1, 2) >>**<< (3, 4)).List, (42, 42), 'and a hyper';
    is &infix:<**>(2, 3), 42, '&infix:<**> in the block is the block-local one';
    is (1, 2).map(-> $x { $x ** 2 }).List, (42, 42), 'a map callback in the block uses it';
    is EVAL('3 ** 2'), 42, 'an EVAL in the block sees it';
    my $x = 2;
    $x **= 3;
    is $x, 42, 'a compound assignment in the block uses it';
}
is-approx third(), 1/3, 'the built-in again after the block';

my &kept = do {
    sub infix:<**>(\a, \b) { "pow:{a}:{b}" }
    -> { 2 ** 3 }
};
is kept(), 'pow:2:3', 'a closure taken out of the block keeps the operator';

# The operator closes over the block's lexicals, per run of the block.
my @seen;
for 1, 10 -> $n {
    sub infix:<*>($a, $b) { $a + $b + $n }
    @seen.push: 3 * 4;
}
is @seen, [8, 17], 'each run of a loop body binds its own operator';

# A routine body is a block too: a nested routine written there sees the
# operator, a routine written outside does not.
sub with-minus($k) {
    sub infix:<->($a, $b) { "minus $a $b $k" }
    sub inner { 5 - 1 }
    (inner(), third(), 5 - 2)
}
my @r = with-minus(7);
is @r[0], 'minus 5 1 7', 'a routine nested in the declaring routine sees the operator';
is-approx @r[1], 1/3, 'a routine written outside it does not';
is @r[2], 'minus 5 2 7', 'the declaring routine itself uses it';
