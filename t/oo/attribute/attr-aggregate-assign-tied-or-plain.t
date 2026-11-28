use Test;

# Assigning to an `@`/`%` attribute inside a method: a plain Array/Hash takes
# the ordinary store, a container class with its own STORE still gets the call
# (the tie probes are skipped only when both stores hold a plain aggregate).

plan 6;

class C {
    has @.r;
    has %.h;
    submethod TWEAK(:@!r --> Nil) { @!r = @!r.map(*.flat) }
    method set-h { %!h = a => 1, b => 2 }
}
is C.new(r => [[1, 2], [3]]).r.map(*.elems).join(','), '2,1', 'TWEAK reassigns a plain array';
is-deeply C.new.r, [], 'an empty array stays empty';
my $c = C.new; $c.set-h;
is-deeply $c.h, %(a => 1, b => 2), 'a plain hash attribute is assigned';

my @stored;
class T does Positional {
    has @.x;
    method STORE(*@a) { @stored = @a; self }
}
class Q {
    has @.q is T;
    method set { @!q = 1, 2 }
}
Q.new.set;
is-deeply @stored, [1, 2], 'a tied attribute container gets STORE';

for ^2 {
    my $o = C.new(r => [[$_]]);
    is $o.r.elems, 1, "repeat $_";
}
