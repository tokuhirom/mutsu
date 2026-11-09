use Test;
use MONKEY-SEE-NO-EVAL;

# A sub reached through a frameless call path inside an EVAL'd unit still has a
# callframe whose line is its call site: the frames `EVAL` itself inserts must
# not be taken for the sub's frame.

plan 3;

my $lines = EVAL q:to/END/;
sub g { callframe(2).line }
sub h { g() }
my @l;
@l.push(h());
@l.push(h());
@l.push(h());
@l;
END
is-deeply $lines.map(*.Int).List, (4, 5, 6), 'callframe(2).line for repeated calls';

my $deeper = EVAL q:to/END/;
sub g { callframe(3).line }
sub h { g() }
sub k { h() }
(k(), k());
END
is-deeply $deeper.map(*.Int).List, (4, 4), 'callframe(3).line two subs out';

is EVAL(q:to/END/).Int, 3, 'the line of the last of two calls';
sub f { callframe(1).line }
f();
f();
END
