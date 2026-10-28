use Test;
use MONKEY-TYPING;

# A dispatch reads a method row's candidates by reference; registering another
# candidate afterwards (a copy on write) must not disturb an earlier dispatch
# nor miss the new candidate.
plan 6;

class C {
    multi method m(Int $x) { "int:$x" }
}
my $c = C.new;
is $c.m(1), 'int:1', 'first candidate dispatches';
augment class C { multi method m(Str $x) { "str:$x" } }
is $c.m('a'), 'str:a', 'a candidate added after a dispatch is found';
is $c.m(2), 'int:2', 'the earlier candidate still wins for its type';

class D {
    multi method new(Str $key, Num $value = 1e0) { self.bless(:$key, :$value) }
    has $.key;
    has $.value;
}
my $d1 = D.new(key => 'k', value => 2e0);
is $d1.value, 2e0, 'named construction bypasses the positional multi new';
my $d2 = D.new('p');
is $d2.key, 'p', 'positional construction reaches the multi new';
is $d2.value, 1e0, 'its default is applied';
