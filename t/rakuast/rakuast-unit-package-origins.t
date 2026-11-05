use Test;

# `unit module Foo;` / `unit package P;` hold the rest of the unit as their
# body, and carry the ranges rakudo 2026.09 gives them (#15780): the statement
# and the declaration run to the end of the unit's last statement, the name and
# the body (block, blockoid, statement list) have their own.

plan 12;

use experimental :rakuast;

sub rng($n) { my $o = $n.origin; $o ?? "{$o.from}-{$o.to}" !! "none" }
sub shape(Str $src) {
    my $u = $src.AST;
    my $st = $u.statements[0];
    my $d = $st.expression;
    my $b = $d.body;
    [rng($st), rng($d), rng($d.name), rng($b), rng($b.body), rng($b.body.statement-list),
     $b.body.statement-list.statements.map(&rng).join(",")]
}

my @m = shape("unit module Foo;\nsub f \{ 1 }\n");
is @m[0], "0-28", 'module: the statement';
is @m[1], "0-28", 'module: the declaration';
is @m[2], "12-15", 'module: the name';
is @m[3], "17-28", 'module: the block';
is @m[5], "17-28", 'module: the statement list';

my @q = shape("unit module Foo::Bar;\nsub f \{ 1 }\nsub g \{ 2 };\n");
is @q[0], "0-46", 'two statements: the statement';
is @q[2], "12-20", 'a qualified name';
is @q[5], "22-46", 'the list includes the terminator';
is @q[6], "22-33,34-45", 'the items';

my @p = shape("unit package P;\nour \$x = 1;\n");
is @p[0], "0-27", 'package: the statement';
is @p[2], "13-14", 'package: the name';
is @p[3], "16-27", 'package: the block';
