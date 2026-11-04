use v6;
use Test;

plan 6;

# `x[i] = v` is carried through finalization as the parser built it
# (ADR-15760 slice 2); the semantics stay those of the ordinary path.

my @a = 1, 2;
@a[0] = 5;
@a[1] += 3;
is @a.raku, '[5, 5]', 'positional element assignment and +=';

my %h;
%h<a> = 1;
%h{'b'} = 2;
is %h.sort.raku, '(:a(1), :b(2)).Seq', 'associative element assignment';

my @m;
@m[0][1] = 7;
is @m.raku, '[[Any, 7],]', 'nested positional autovivification';

%h<x><y> = 4;
is %h<x>.raku, '${:y(4)}', 'nested associative autovivification';

@a[3] = @a[2] = 8;
is @a.raku, '[5, 5, 8, 8]', 'chained element assignment';

@a[4, 5] = 1, 2;
is @a.elems, 6, 'slice assignment';
