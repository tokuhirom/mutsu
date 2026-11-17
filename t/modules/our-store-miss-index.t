use v6;
use Test;

# The `our` store answers a miss for a name it never held from its
# unqualified-name index. Every way a stored key is reached must still find it:
# the plain qualified key, a lexical package's member through its storage
# name, a package block's constant read from its routine after the block, and
# type checks against builtin names whose term keys (`\Real`) are never stored.

plan 7;

package Outer {
    our $count = 3;
    our constant LIMIT = 10;
    our sub limit() { LIMIT }
    our sub bump() { $count++ }
}
is $Outer::count, 3, 'a package scalar by its qualified name';
Outer::bump();
is $Outer::count, 4, 'written through the package routine';
is Outer::limit(), 10, "a package block's constant from its routine";

my package Lex { our $v = 'lexical member' }
is $Lex::v, 'lexical member', "a lexical package's member through its storage name";

sub typed(Real $r, Str $s, Date $d --> Str) { "$r $s {$d.year}" }
is typed(1.5, 'x', Date.new(2026, 11, 17)), '1.5 x 2026', 'builtin-typed parameters';
is (1..20).map({ typed($_, 'y', Date.new(2000, 1, 1)) }).tail, '20 y 2000',
    'the same type checks in a loop';

enum Period <MONTHLY WEEKLY>;
sub period-name(Period $p) { $p eq MONTHLY ?? 'm' !! 'w' }
is (MONTHLY, WEEKLY, MONTHLY).map(&period-name).join, 'mwm', 'an enum member read as a bare word';
