# Class-body `my` statics read and written from methods: a frame that
# adopts the class's pre-built statics overlay behaves as one that inserted
# each static (#18274).
use Test;
class C {
    my $count = 0;
    my @log;
    my %seen;
    my \term = 'T';
    method bump { $count++; @log.push($count); %seen{$count} = True; $count }
    method read { $count }
    method shadow($count) { $count }
    method closure { -> { $count + 100 } }
    method term-read { term }
    method both($x) { $x ~ term ~ $count }
}
my $c = C.new;
is $c.read, 0, 'a static read in a fresh frame';
is $c.bump, 1, 'a static written in a method';
is $c.bump, 2, 'and the write persists to the next call';
is $c.read, 2, 'another method sees it';
is $c.shadow(42), 42, 'a parameter of the same name wins over the static';
is $c.read, 2, 'the static is untouched by the shadowing parameter';
my &cl = $c.closure;
is cl(), 102, 'a closure created in a method captures the static';
$c.bump;
is $c.closure()(), 103, 'a fresh closure sees the new value';
is $c.term-read, 'T', 'a sigilless static';
is $c.both('x'), 'xT3', 'parameter and statics together';
for ^5 { $c.bump }
is $c.read, 8, 'repeated calls';
done-testing;
