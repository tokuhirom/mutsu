use Test;

# A method whose body creates closures does not write `self`, `?CLASS` and
# `?ROLE` into its frame env either (ADR-14633, ADR-14792): a closure takes
# the names it reads from the frame's record when it is created, so a later
# call sees its creator's names, not those of the method it runs under.

plan 9;

role R { method make-role { -> { ::?ROLE.^name } } }

class Maker does R {
    has $.tag = 'maker';
    method make-self { -> { self.tag } }
    method make-class { -> { ::?CLASS.^name } }
    method make-dyn { my $t = $!tag; -> $x { "$t:$x:" ~ self.^name } }
}

class Runner {
    has $.tag = 'runner';
    method run(&c) { c() }
    method run1(&c, $x) { c($x) ~ '/' ~ self.tag ~ '/' ~ ::?CLASS.^name }
}

my $m = Maker.new;
my $r = Runner.new;

is $r.run($m.make-self), 'maker', "a closure keeps its creator's self under another method";
is $r.run($m.make-class), 'Maker', "a closure keeps its creator's ::?CLASS";
is $r.run($m.make-role), 'R', "a closure keeps its creator's ::?ROLE";
is $r.run1($m.make-dyn, 'x'), 'maker:x:Maker/runner/Runner',
    'the runner still sees its own self and class around the call';

# The closure is called after its creating method returned, with no method
# running at all.
my &later = $m.make-self;
is later(), 'maker', 'called with no method frame';

# A subclass instance: the closure's self is the instance, ::?CLASS the
# declaring class.
class Sub is Maker { }
my $s = Sub.new(tag => 'sub');
is $r.run($s.make-self), 'sub', 'closure from an inherited method sees the instance';
is $r.run($s.make-class), 'Maker', '::?CLASS is the declaring class';

# A method declaring a named sub that reads self after the method returned.
class WithSub {
    has $.x = 7;
    method get-f { sub F() { self.x }; &F }
}
my &f = WithSub.new.get-f;
is f(), 7, 'a named sub declared in a method keeps self';

# Nested creation: a closure made inside a closure.
class Nest {
    has $.n = 'nest';
    method outer { -> { -> { self.n } } }
}
is $r.run(Nest.new.outer.()), 'nest', 'a closure created by a closure keeps self';
