use v6;
use Test;

plan 15;

# A `--> 'literal'` return spec yields that string. A plain single-quoted
# literal is built directly; one with an escape still goes through EVAL.
sub empty(--> '') { Nil }
sub word(--> 'xy') { Nil }
sub spaced(--> 'a b') { Nil }
sub escaped(--> 'a\0b') { Nil }
sub quoted(--> 'it\'s') { Nil }

is empty().raku, '""', 'empty literal';
is empty().WHAT.^name, 'Str', 'it is a Str';
is empty().chars, 0, 'of length 0';
is word(), 'xy', 'a word';
is spaced(), 'a b', 'a literal with a space';
is escaped().chars, 4, 'a literal with a backslash keeps its text';
is escaped().substr(1, 2), '\\0', 'the backslash is kept';
is quoted(), q[it's], 'a literal with an escaped quote';

class K {
    multi method m(Int:D $ --> '') { }
    multi method m(Str:D $ --> 'lit') { 5 }
    multi method m(Num:D $x --> 'num') { $x }
}
my $k = K.new;
is $k.m(1), '', 'a multi method with an empty literal return';
is $k.m('s'), 'lit', 'a multi method returning its literal over a value body';
is $k.m(2e0), 'num', 'a multi method returning its literal over a body that returns a value';

# Repeated calls return a fresh equal value each time.
my @r = (^3).map: { word() };
is @r.join(','), 'xy,xy,xy', 'repeated calls';
ok empty() eq '', 'eq the empty string';
ok empty().defined, 'the result is defined';
nok empty().Bool, 'and falsey';
