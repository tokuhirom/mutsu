use Test;

# A Method object from `.^find_method` / `.^lookup` invoked on an invocant
# (`$obj.$m(...)`, `$m($obj, ...)`) runs exactly its own candidate, with the
# arguments it is given -- the value-call entry calls it without the instance
# cascade in front of `CALL-ME`.

plan 13;

class Parent {
    has $.log = '';
    method greet($who = 'world', :$loud) { $loud ?? "HELLO {$who.uc}" !! "hello $who" }
    method who { 'parent' }
    method rec(*@a, *%h) { @a.elems ~ '/' ~ %h.keys.sort.join(',') }
}
class Child is Parent {
    method who { 'child' }
}

my $p = Parent.new;
my $c = Child.new;

my $greet = Parent.^find_method('greet');
is $p.$greet(), 'hello world', 'no arguments: the default applies';
is $p.$greet('raku'), 'hello raku', 'a positional argument';
is $p.$greet('raku', :loud), 'HELLO RAKU', 'a named argument';
is $greet($p, 'x'), 'hello x', 'called as a routine with the invocant first';

my $pwho = Parent.^find_method('who');
is $c.$pwho(), 'parent', "the owner's candidate, not the invocant's override";
is Child.^find_method('who')($c), 'child', "the subclass's own candidate";
is $c.$pwho, 'parent', 'without parentheses';

my $rec = Parent.^lookup('rec');
my $cap = \(1, 2, 3, :a, :b(2));
is $p.$rec(|$cap), '3/a,b', 'a capture flattens into the call';
is $p.$rec(), '0/', 'slurpies bind empty';

for ^3 -> $i {
    is $c.$greet("n$i"), "hello n$i", "repeated call $i";
}

my $mm = Parent.^find_method('nope');
nok $mm.defined, 'a missing method is not found';
