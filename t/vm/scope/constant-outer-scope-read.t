use Test;

# A sigil-less constant of an enclosing scope, read from a nested routine or
# closure, is a captured lexical read of the constant's term key -- not the
# run-time bareword resolution (App::Moneymoor's `valid-date` reads a module
# `my constant DAYS-IN-MONTH` from an exported sub; #17265).

plan 12;

use lib $*PROGRAM.parent(3).add('lib-constant-outer-scope');
use ConstantOuterScope;

is days-in(2), 28, 'a module sub reads the module my constant';
is days-in(12), 31, 'and indexes it';
is max-periods(), 1200, 'an our-scoped module constant';
my $sum = 0;
$sum += days-in(($_ % 12) + 1) for ^120;
is $sum, 3650, 'many reads';

my constant LIST = (10, 20, 30);
constant OURC = <a b c>;
sub at(Int $i) { LIST[$i] }
is at(1), 20, 'a file sub reads a file my constant';
sub ours() { OURC.join }
is ours(), 'abc', 'and an our constant';

sub shadow() { my $LIST = 5; LIST.elems }
is shadow(), 3, 'a same-named $-scalar does not hide the constant';

sub own() { my constant LIST = (1,); LIST.elems }
is own(), 1, 'an inner constant shadows the outer one';

sub param(\LIST) { LIST }
is param(42), 42, 'a sigilless parameter shadows the outer constant';

my &later;
{
    my constant INNER = (7, 8);
    &later = sub () { INNER[1] };
}
is later(), 8, 'an escaping closure keeps the constant of its scope';

sub nested() { my $f = -> $i { LIST[$i] }; $f(2) }
is nested(), 30, 'a pointy block nested in a sub reads it too';

class C { method m() { LIST[0] } }
is C.m, 10, 'a method reads a file constant';
