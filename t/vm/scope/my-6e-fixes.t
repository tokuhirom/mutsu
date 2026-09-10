use v6.e.PREVIEW;
use Test;

plan 11;

# Anonymous `my &` defaults to the Callable type object (not Any/Nil).
ok (my &) eqv Callable, 'anonymous my & is Callable';
is (my &foo).^name, 'Callable', 'named my & default is Callable';

# Anonymous `my sub {...}` can be immediately called.
is (my sub {42})(), 42, 'my sub {42}() immediate call';
is my sub {7}(), 7, 'my sub {7}() without parens around the sub';

# A `my` declared inside EVAL must NOT leak into the caller pad.
EVAL 'my $leak1 = 99';
nok $leak1.defined, 'EVAL my scalar does not leak';
EVAL 'loop (my $leak2 = 1; 0; 1) { }';
nok $leak2.defined, 'EVAL loop-init my does not leak';
EVAL 'if my $leak3 = 1 { } else { }';
nok $leak3.defined, 'EVAL if-cond my does not leak';

# But EVAL can still read + assign the caller's existing lexicals.
my $outer = 10;
EVAL '$outer = 20';
is $outer, 20, 'EVAL assignment to outer lexical persists';

# Use-before-declaration in the same statement is X::Undeclared.
my $ret = 42;
throws-like '$ret = $foo ~ my $foo;', X::Undeclared,
    'use of $foo before its same-statement my is X::Undeclared';

# Calling an undeclared routine inside EVAL dies before side effects run.
my $bad = 0;
dies-ok { EVAL '$bad = 1; no_such_routine_xyz()' }, 'undeclared routine in EVAL dies';
nok $bad, '... and it does so before run time';
