use Test;
use nqp;

# From Iter::Able (Min-Max): `nqp::until(cond, body, post)` runs `post` after
# each body; an already-true condition runs neither.
plan 4;

my @log;
my $n = 0;
nqp::until(1 == 1, @log.push('body'), @log.push('post'));
is @log.elems, 0, 'true condition runs neither body nor post';
nqp::until($n >= 2, ($n++, @log.push("b$n")), @log.push("p$n"));
is @log.join(','), 'b1,p1,b2,p2', 'post follows every body';
@log = ();
nqp::while($n < 4, ($n++, @log.push("b$n")), @log.push("p$n"));
is @log.join(','), 'b3,p3,b4,p4', 'nqp::while takes a post step too';

sub first-concrete($it, $first is rw) {
    nqp::stmts(
        nqp::until(
            nqp::eqaddr((my $pulled := $it.pull-one), IterationEnd),
            Nil,
            nqp::if(nqp::isconcrete($pulled), nqp::stmts(($first = $pulled), (return $it)))
        ),
        Mu)
}
my $min;
first-concrete([].iterator, $min);
nok $min.defined, 'an empty iterator leaves the out-parameter untouched';
