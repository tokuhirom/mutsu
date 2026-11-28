use Test;

# A BUILD/TWEAK with a body, whose parameters are the invocant and optional
# untyped attributive named parameters, runs in the lean method frame
# (`vm_phase_step_lean`). Every observable effect of the general binder must
# stay: the attributive binds, the body's closures and their topic, writes to
# outer lexicals and dynamic variables, `fail`, and the shapes the lean frame
# refuses.

plan 15;

class D {
    has @.resources;
    has %!meta;
    has $.seen;
    submethod TWEAK(:%!meta, :@!resources --> Nil) {
        @!resources = @!resources.map(*.flat);
        $!seen = %!meta<k> // 'none';
    }
}
for ^3 {
    my $d = D.new(resources => (1, (2, 3)), meta => %(k => "v$_"));
    is-deeply ($d.resources.elems, $d.resources.map(*.List).List, $d.seen),
        (2, ((1,), (2, 3)), "v$_"), "replay $_: attributive binds and a closure in the body";
}
is-deeply D.new.resources, [], 'unpassed attributive params bind the empty container';
is D.new.seen, 'none', 'the body reads an unpassed %! param as empty';

# Closures over the frame's lexicals, and their topic.
class C {
    has @.out;
    submethod TWEAK(:@!out) {
        my $offset = 10;
        @!out = @!out.map({ $_ + $offset });
        my &f = -> $x { $x * $offset };
        @!out.push: f(2);
    }
}
is-deeply C.new(out => (1, 2)).out, [11, 12, 20], 'closures see the frame lexicals and their own topic';
is-deeply C.new(out => (5,)).out, [15, 20], 'and again on a replay';

# Writes to an outer lexical and a dynamic variable reach the caller.
my $count = 0;
class W {
    has $.v;
    submethod TWEAK(:$!v) { $count += $!v; $*SEEN = $!v if $*SEEN.defined }
}
W.new(v => $_) for 1..3;
is $count, 6, 'an outer lexical written by the TWEAK body';
my $*SEEN = 0;
W.new(v => 9);
is $*SEEN, 9, 'a dynamic variable written by the TWEAK body';

# A `fail` in TWEAK surfaces as before.
class F {
    has $.x;
    submethod TWEAK(:$!x) { fail "bad x" if $!x < 0 }
}
lives-ok { F.new(x => 1) }, 'a TWEAK that does not fail';
dies-ok { F.new(x => -1).x }, 'a TWEAK that fails';

# Shapes the lean frame refuses still run on the general binder.
class Typed { has Int $.i; has $.d; submethod TWEAK(Int :$!i) { $!d = $!i * 2 } }
is Typed.new(i => 4).d, 8, 'a typed attributive param';
throws-like { Typed.new(i => 'x') }, X::TypeCheck, 'and its type check';
class Def { has $.x; has $.y; submethod TWEAK(:$!x = 3) { $!y = $!x + 1 } }
is Def.new.y, 4, 'a defaulted attributive param';
class Pos { has $.y; submethod TWEAK(:$foo) { $!y = $foo // 'nofoo' } }
is Pos.new(foo => 'f').y, 'f', 'a plain named param';
