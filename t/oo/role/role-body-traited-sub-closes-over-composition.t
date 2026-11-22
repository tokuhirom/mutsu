use Test;

# A role-body `sub` with a custom trait closes over each composition's scope
# -- the role's lexicals and its parameters -- like an untraited one, while
# its trait still runs once (#18721, #18097).

plan 5;

my $code = q:to/CODE/;
multi trait_mod:<is>(Routine $r, :$probe!) { note "probe {$r.name}" }
my role P[$x] { sub h is probe { $x }; method hx { h() }; method hy { &h() } }
class E does P[1] { }
class F does P[2] { }
role Q { my $v = 'qv'; sub g is probe { $v }; method m { g() } }
class G does Q { }
say E.new.hx, ' ', F.new.hx, ' ', E.new.hy, ' ', F.new.hy, ' ', G.new.m;
CODE

my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
my $out = $proc.out.slurp(:close);
my @probes = $proc.err.slurp(:close).lines;
is $out, "1 2 1 2 qv\n", 'each composition reads its own parameter and the role lexical';
is @probes.grep('probe h').elems, 1, 'the parametric role\'s sub was traited once';
is @probes.grep('probe g').elems, 1, 'and so was the plain role\'s';

# A trait that wraps the routine keeps the role's one wrapped routine.
multi trait_mod:<is>(Routine $r, :$wrapped!) { $r.wrap(-> |c { 'wrapped' }) }
role W { my $v = 'plain'; method bare { f() }; sub f is wrapped { $v } }
class K does W { }
is K.new.bare, 'wrapped', 'a wrapped sub stays wrapped in the composition';

role M { my $v = 'mixed'; method m { s() }; sub s is pure { $v } }
class L does M { }
is L.new.m, 'mixed', 'a built-in trait keeps the closure';
