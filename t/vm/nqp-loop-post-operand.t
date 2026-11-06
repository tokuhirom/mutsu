use Test;
use nqp;

# `nqp::while` / `nqp::until` take an optional third operand, run after the body of
# every iteration (QAST's `post`); it was refused (#16939).

plan 6;

my int $i = 0;
my @seen;
nqp::until(nqp::iseq_i($i, 3), $i++, @seen.push($i));
is @seen.join(','), '1,2,3', 'until: post runs after each body';

my int $j = 0;
my @w;
nqp::while(nqp::islt_i($j, 3), @w.push("b$j"), $j++);
is @w.join(','), 'b0,b1,b2', 'while: post as the step';

my int $k = 5;
my $ran = 0;
nqp::until(nqp::iseq_i($k, 5), $ran++, $ran++);
is $ran, 0, 'a satisfied condition runs neither body nor post';

sub first-concrete($it, $first is rw, $index is rw) {
    nqp::stmts(
        (my int $idx = -1),
        nqp::until(
            nqp::eqaddr((my $pulled := $it.pull-one), IterationEnd),
            $idx++,
            nqp::if(
                nqp::isconcrete($pulled),
                nqp::stmts(($index = $idx), ($first = $pulled), (return $it))
            )
        ),
        Mu
    )
}
my $empty = [].iterator;
ok !first-concrete($empty, my $f1, my int $x1), 'empty iterator: falsy';
my $it = [Int, 7, 8].iterator;
ok first-concrete($it, my $f2, my int $x2), 'non-empty: iterator returned';
is "$f2/$x2", '7/1', 'first concrete value and its index';
