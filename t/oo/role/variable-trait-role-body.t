use Test;

# A variable trait directly in a role body runs its handler once per role, while
# the role is compiled, with `Variable.block` a Sub (a role body is a Sub), not
# once per composition (#14532). Expected values are the `raku` output.

plan 6;

my @log;
multi trait_mod:<is>(Variable:D $v, :$foo!) { @log.push: "{$v.name}:{$v.block.^name}" }

role R { my $rv is foo; method x { 1 } }
class A does R {}
class B does R {}
class C does R {}

role P[::T] { my $pv is foo; method t { T.^name } }
class D does P[Int] {}
class E does P[Str] {}

role W { my $wv is foo; my $plain = 3; method p { $plain } }
class F does W {}

is @log.elems, 3, 'one handler call per role, not per composition';
is @log.sort.join(','), '$pv:Sub,$rv:Sub,$wv:Sub', 'each with its variable and a Sub block';
is A.new.x + B.new.x + C.new.x, 3, 'the role still composes and works';
is D.new.t ~ E.new.t, 'IntStr', 'a parametric role still binds its parameter per composition';
is F.new.p, 3, 'an ordinary variable beside it is unaffected';

multi trait_mod:<is>(Variable:D $v, :$bar!) { }
role I { my $iv is bar = 4; method v { $iv } }
class G does I {}
is G.new.v, 4, 'a variable with an initializer keeps its value';
