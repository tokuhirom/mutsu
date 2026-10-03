use Test;

# `--> Hash:D[Int:D]` is the same type as `--> Hash[Int:D]:D`: the smiley may
# precede the parametrization. From Benchmark's
# `sub timethis(... --> Hash:D[Duration:D])` returning `my Duration:D %result`.

plan 3;

sub f(--> Hash:D[Int:D]) { my Int:D %r = a => 1; %r }
is-deeply f(), (my Int:D % = a => 1), 'a typed hash satisfies the return type';
is &f.returns.raku, 'Hash[Int:D]:D', 'canonical spelling';

sub h(--> Array:D[Str]) { my Str @a = <x y>; @a }
is h().join(','), 'x,y', 'Array:D[Str]';
