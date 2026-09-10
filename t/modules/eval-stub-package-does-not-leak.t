use Test;

plan 8;

# An `EVAL` is its own compilation unit, so a class/package it stubs
# (`class A { ... }`) and never defines dies with that unit. It must not stay
# in the enclosing program's stub registry, where the end-of-run check would
# report "The following packages were stubbed but not defined" for a name the
# outer program never mentioned -- and make the process exit non-zero after
# every test had already passed.

# An EVAL that dies BEFORE its own end-of-unit stub check (here: composing a
# class against the still-open stub) must leave nothing behind.
{
    my $r = try EVAL 'class A1Leak { ... }; class B1Leak does A1Leak { };';
    ok !$r.defined, 'the EVAL itself failed';
    pass 'and the program is still running after it';
}

# The same for a package stub.
{
    try EVAL 'package P1Leak { ... }; package Q1Leak is P1Leak { };';
    pass 'a leaked package stub does not abort the program either';
}

# An EVAL whose snippet SUCCEEDS but leaves a stub open still reports it as
# that EVAL's own error, exactly like raku -- this is the behaviour the
# no-leak rule must not break.
{
    try EVAL 'class A2Leak { ... }';
    ok $!.defined, 'an EVAL that only stubs a class errors';
    like $!.message, /'stubbed but not defined'/, 'and says so';
}

# A name a previous EVAL stubbed must still BE a stub for the class system, so
# a later EVAL that re-stubs it and inherits from it still refuses. (Clearing
# the registry entry outright instead of just spending its error made this
# silently succeed -- `roast/S12-class/stubs.t` test 7.)
{
    try EVAL 'class A5Twice { ... }; A5Twice.WHAT';
    try EVAL 'class A5Twice { ... }; class B5Twice is A5Twice {}';
    ok $!.defined, 're-stubbing a name a previous EVAL stubbed still errors';
    ok $! ~~ X::Inheritance::NotComposed,
        'and it is the inheritance error, not a stub-registry artifact';
}

# A stub the OUTER program opens and closes itself is unaffected by an EVAL
# running in between.
class A3Outer { ... }
try EVAL 'class A4Inner { ... }';
class A3Outer { method m { 'defined' } }
is A3Outer.new.m, 'defined', 'an outer stub completed after an EVAL still works';
