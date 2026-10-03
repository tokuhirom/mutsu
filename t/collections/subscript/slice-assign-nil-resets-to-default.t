use Test;
# From Terminal::UI (Terminal::ANSI::Virtual.scroll-down): `@!chars[$top..$bot] = (Nil, |...)`.
plan 4;
my @a = 1, 2, 3; @a[0..1] = (Nil, 5);
is-deeply @a.raku, '[Any, 5, 3]', 'range slice: Nil stores Any';
my @b = 1, 2, 3; @b[0, 1] = Nil, 5;
is-deeply @b.raku, '[Any, 5, 3]', 'list slice: Nil stores Any';
my %h; %h<a b> = (Nil, 1);
is-deeply %h<a>, Any, 'hash slice: Nil stores Any';
my @d = 1, 2, 3; @d[0..1] = (Nil, |@d[0..^1]);
is-deeply @d.raku, '[Any, 1, 3]', 'scroll-down idiom';
