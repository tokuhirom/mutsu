use v6;
use Test;
use lib 't/lib';
use EvalContext;

plan 3;

my $plain = (role PlainRoleExpr { method plain-marker { 1 } });
is $plain.plain-marker, 1,
    'a role declaration expression yields a usable individual role';

role SameNameRoleExpr { }
my $marker = run-plain(
    q[(1 but (role SameNameRoleExpr { method fresh-marker { 42 } })).fresh-marker]
);
is $marker, 42,
    'EVAL in another unit yields the newly declared same-named role';

is SameNameRoleExpr.^methods.elems, 0,
    'the outer same-named role remains untouched';
