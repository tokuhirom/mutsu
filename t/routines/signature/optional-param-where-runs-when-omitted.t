use v6;
use Test;

plan 20;

# An optional parameter's `where` post-constraint runs even when the argument is
# omitted: rakudo binds the parameter to the value it would get (the nominal type
# object, or the evaluated default) and tests the constraint against that. mutsu
# used to skip the constraint entirely on the omitted-argument path, so
# `sub f($x? where { $_ ~~ Int }) { }` happily accepted `f()` (#8089).

# --- bare optional, all four supplied/omitted x passing/failing corners --------

{
    sub f($x? where { $_ ~~ Int }) { 'ran' }
    is f(3), 'ran', 'supplied value satisfying the constraint binds';
    dies-ok { f('s') }, 'supplied value failing the constraint dies';
    dies-ok { f() },    'omitted arg fails the constraint (binds Any, not Int)';
}

{
    sub f($x? where { True }) { $x.^name }
    is f(), 'Any', 'omitted arg passing the constraint binds the Any type object';
}

# The value under test is the parameter's *declared* type object, not always Any.
{
    sub f(Int $x? where { $_.^name eq 'Int' }) { 'ran' }
    is f(), 'ran', 'typed optional: constraint sees the declared type object';
}
{
    sub f(Int $x? where { $_.^name eq 'Any' }) { 'ran' }
    dies-ok { f() }, 'typed optional: the type object is Int, not Any';
}

# `@?`/`%?` bind a fresh empty container rather than a type object.
{
    sub f(@x? where { $_.^name eq 'Array' && $_.elems == 0 }) { 'ran' }
    is f(), 'ran', 'omitted @-optional: constraint sees an empty Array';
}
{
    sub f(%h? where { $_.^name eq 'Hash' && $_.elems == 0 }) { 'ran' }
    is f(), 'ran', 'omitted %-optional: constraint sees an empty Hash';
}

# --- the anonymous optional, which binds nothing but is still checked ---------

{
    sub f($? where { $_ ~~ Int }) { 'ran' }
    dies-ok { f() }, 'anonymous optional runs its constraint when omitted';
    is f(1), 'ran', 'anonymous optional accepts a satisfying argument';
}

# The failure names the parameter the way rakudo does, not by mutsu's internal
# placeholder for an anonymous parameter.
{
    sub f($? where { False }) { 'ran' }
    my $msg = '';
    try { f(); CATCH { default { $msg = .message } } };
    like $msg, /'<anon>'/, 'anonymous parameter is reported as <anon>';
    unlike $msg, /ANON/, 'the internal placeholder name does not leak';
}

# --- a parameter whose `where` precedes a default -----------------------------
# (`$x = 3 where {...}` is a compile-time error in rakudo, so this spelling is
# the only one, and the constraint applies to the default value.)

{
    sub f($x where { $_ ~~ Int } = 3) { 'ran' }
    is f(), 'ran', 'defaulted param: constraint runs against the default and passes';
}

{
    sub f($x where { $_ ~~ Str } = 3) { 'ran' }
    dies-ok { f() }, 'defaulted param: constraint runs against the default and fails';
}

# --- dispatch must agree with binding ----------------------------------------
# A `where` clause is a multi-dispatch discriminator, so a skipped constraint
# silently widens candidate selection. If the matcher and the binder disagree, a
# candidate is selected and then dies binding the very call it was selected for.

{
    multi g($x, $y? where { $_ ~~ Int }) { 'constrained' }
    multi g($x, $y?)                     { 'plain' }
    is g(1),    'plain',       'omitted arg: constrained candidate is not selected';
    is g(1, 2), 'constrained', 'supplied Int: constrained candidate wins';
    is g(1, 'a'), 'plain',     'supplied Str: falls through to the plain candidate';
}

{
    multi g($x, $y? where { $_ ~~ Int }) { 'constrained' }
    dies-ok { g(1) }, 'no candidate remains once the omitted-arg constraint fails';
}

# The shape that motivated the ticket: a trailing anonymous optional used purely
# as a platform guard (Digest::xxHash's `$? where { $*KERNEL.bits == 64 }`).
{
    multi pick(Int $seed = 0) { 32 }
    multi pick(Int $seed = 0, $? where { False }) { 64 }
    is pick(7), 32, 'trailing anonymous where-guard does not match when it is False';
}

{
    multi pick(Int $seed = 0) { 32 }
    multi pick(Int $seed = 0, $? where { True }) { 64 }
    is pick(7), 64, 'trailing anonymous where-guard matches when it is True';
}
