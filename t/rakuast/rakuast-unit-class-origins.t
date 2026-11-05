use Test;

# `unit class C;` / `unit role R;` / `unit grammar G;` hold the rest of the
# unit as their body, and carry the ranges rakudo 2026.09 gives them (#15780):
# the unit's list, the statement and the declaration run to the end of the
# last statement, the name and the body (block, blockoid, list) have their own.

plan 20;

use experimental :rakuast;

sub rng($n) { my $o = $n.origin; $o ?? "{$o.from}-{$o.to}" !! "none" }
sub shape(Str $src) {
    my $u = $src.AST;
    my $st = $u.statements[0];
    my $d = $st.expression;
    my $b = $d.body;
    [rng($u), rng($st), rng($d), rng($d.name), rng($b), rng($b.body),
     rng($b.body.statement-list)]
}

my @c = shape("unit class C;\nhas \$.x;\nmethod m \{ 1 }\n");
is @c[0], "0-37", 'class: the unit list';
is @c[1], "0-37", 'class: the statement';
is @c[2], "0-37", 'class: the declaration';
is @c[3], "11-12", 'class: the name';
is @c[4], "14-37", 'class: the block';
is @c[6], "14-37", 'class: the statement list';

my @t = shape("unit class A::B is Int;\nmethod m \{ 1 };\n\n");
is @t[1], "0-39", 'trait and terminator: the statement';
is @t[3], "11-15", 'a qualified name';
is @t[4], "24-39", 'the block includes the terminator';

my @r = shape("unit role R;\nmethod m \{ 1 }\n");
is @r[1], "0-27", 'role: the statement';
is @r[3], "10-11", 'role: the name';
is @r[4], "10-27", 'role: the body starts at the name';
is @r[5], "13-27", 'role: the blockoid';

my @p = shape("unit role R[::T];\nmethod m \{ 1 }\n");
is @p[4], "10-32", 'parametric role: the body';
is @p[5], "18-32", 'parametric role: the blockoid';

my @g = shape("unit grammar G;\ntoken t \{ a }\n");
is @g[1], "0-29", 'grammar: the statement';
is @g[3], "13-14", 'grammar: the name';
is @g[4], "16-29", 'grammar: the block';

is shape("unit module M;\nsub f \{ 1 }\n")[0], "0-26", 'unit module list unchanged';
is shape("unit class D;\nmethod m \{ 1 }\n")[0], "0-28", 'class: a single statement';
