use Test;

plan 7;

my @t = 0, 10;
@t[0,1]:v = 31, 32;
is-deeply @t, [31, 32], 'array slice :v assigns through to the source';

my %h = a => 1, b => 2;
%h<a b>:v = 5, 6;
is-deeply %h, {a => 5, b => 6}, 'hash slice :v assigns through to the source';

my @u = 0, 10;
my $l = @u[0,1]:v;
$l[0] = 5;
is-deeply @u, [5, 10], 'element of a captured slice :v writes the array';

is-deeply @u[0,1]:v, (5, 10), 'read-only slice :v yields plain values';
is-deeply @u[0,1,5]:v, (5, 10), 'missing index is skipped';
is-deeply %h<a b z>:v, (5, 6), 'missing key is skipped';
is (@u[0,1]:v).raku, '(5, 10)', 'containers do not leak into .raku';
