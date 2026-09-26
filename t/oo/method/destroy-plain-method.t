use Test;

# A class's DESTROY may be declared with `method` as well as `submethod`:
# rakudo's destroyer list takes each MRO class's own DESTROY either way. mutsu
# used to run only the submethod spelling, so `method DESTROY` never fired.

plan 4;

my @events;

class Decl { method DESTROY { @events.push("decl") } }
my $d = Decl.new;
$d = Nil;
quietly $*VM.request-garbage-collection;
is-deeply @events, ["decl"], 'a plain `method DESTROY` runs';

class Base { method DESTROY { @events.push("base") } }
class Child is Base { }
my $c = Child.new;
$c = Nil;
quietly $*VM.request-garbage-collection;
is-deeply @events, ["decl", "base"], 'an inherited `method DESTROY` runs once, for its own class';

class Child2 is Base { submethod DESTROY { @events.push("child2") } }
my $c2 = Child2.new;
$c2 = Nil;
quietly $*VM.request-garbage-collection;
is-deeply @events, ["decl", "base", "child2", "base"], 'a submethod and a parent method both run, child first';

class Late { }
Late.^add_method('DESTROY', anon method DESTROY { @events.push("late") });
Late.^compose;
my $l = Late.new;
$l = Nil;
quietly $*VM.request-garbage-collection;
is @events.tail, 'late', 'a DESTROY method added through the metamodel runs';
