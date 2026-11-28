use Test;

# An empty-bodied BUILD/TWEAK whose parameters are all attributive named
# parameters binds them without a method frame (`ctor_phase_bind_only`).
# Every observable effect of the full binder must stay: unpassed parameters
# reset the attribute to the type object, `@!`/`%!` parameters re-home their
# value, an explicit Nil stores the container default, a later phase step
# sees the bound value, BUILD's write suppresses the attribute initializer,
# and the shapes the bind-only path refuses still run through the frame.

plan 16;

class A {
    has $.x = 5;
    has @.a = 1, 2;
    has %.h = a => 1;
    submethod TWEAK(:$!x, :@!a, :%!h) { }
}
is A.new.raku, 'A.new(x => Any, a => [], h => {})', 'unpassed attributive params reset to the type object';
my $a = A.new(x => 1, a => (3, 4), h => { b => 2 });
is $a.x, 1, 'scalar attributive param bound';
is-deeply $a.a, [3, 4], '@! param re-homes a List into the Array';
is-deeply $a.h, { b => 2 }, '%! param bound';
is A.new(x => 1, x => 2).x, 2, 'the last of duplicate named args wins';

class Spec { has $.spec; submethod TWEAK(:$!spec) { } }
class Dist is Spec {
    has $.seen;
    submethod TWEAK() { $!seen = self.spec }
}
is Dist.new(spec => 'S').seen, 'S', 'a later phase step sees the attribute the bind-only step bound';
is Dist.new(spec => 'S').spec, 'S', 'inherited bind-only step binds the parent attribute';

class B { has $.x = 7; submethod BUILD(:$!x) { } }
is B.new(x => 3).x, 3, 'BUILD bind-only binds';
is B.new.x.raku, 'Any', 'BUILD writing the type object suppresses the initializer';

class D { has $.x is default(42); submethod TWEAK(:$!x) { } }
is D.new(x => Nil).x, 42, 'an explicit Nil stores the attribute container default';

my $c = 0;
class P { has $.p; submethod TWEAK(:$!p) { } }
my $item = [1, 2];
my $p = P.new(p => $item);
is-deeply $p.p, [1, 2], 'an Array value binds into a scalar attribute';

# Shapes the bind-only path refuses: they keep the frame.
class Typed { has Int $.i = 3; submethod TWEAK(:$!i) { } }
throws-like { Typed.new(i => 's') }, X::TypeCheck, 'a typed attribute still rejects a bad value';
class Def { has $.x; submethod TWEAK(:$!x = 4) { } }
is Def.new.x, 4, 'a defaulted attributive param still evaluates its default';
class Req { has $.x; submethod TWEAK(:$!x!) { } }
dies-ok { Req.new }, 'a required attributive param still dies when not passed';
class Body { has $.x; has $.y; submethod TWEAK(:$!x) { $!y = $!x * 2 } }
is Body.new(x => 5).y, 10, 'a step with a body still runs it';

# A wrapped TWEAK must run its wrapper.
class W { has $.w; submethod TWEAK(:$!w) { } }
W.^find_method('TWEAK').wrap(-> |c { $c++; callsame });
W.new(w => 1);
is $c, 1, 'a wrapped bind-only step runs its wrapper';
