use v6;
use Test;
use lib 't/lib';
use RoleSubParamLexical;

# #11528: a role parameterised on a Sub keeps its body lexicals (an INIT
# lexical and a plain one) for methods reached from outside the composing
# frame. Its pun class is stored under a name that carries the Sub's .WHICH,
# and method dispatch must look the lexicals up under that same name. The
# fixture mirrors upstream NativeCall's `Native` role (#11203). The first three
# tests rely on mutsu running traits at run time: rakudo runs the trait at
# compile time and then finishes compiling the routine, which restores its
# declared body.

plan 4;

sub f() is wrapped { 'declared body' }

is f(), 'Lock plain calls=1 name=f',
    'the rebound body sees the role-body lexicals and the role attributes';
is f(), 'Lock plain calls=2 name=f',
    'a private method\'s attribute write is seen by the next call';
is &f.name, 'f', 'a private role attribute $!name is not an accessor (the name follows $!do)';

my role Local[$p] {
    my $x = 'x';
    my $y = 'y';
    method get() { "$x$y" }
}
sub g() { }
&g does Local[&g];
is &g.get, 'xy', 'a role parameterised on a Sub reads its body lexicals';
