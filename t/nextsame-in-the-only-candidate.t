use v6;
use Test;

# A `multi` with a single candidate is still a dispatcher: it merely has no
# NEXT candidate. `nextsame`/`callsame`/`nextwith`/`callwith` there are legal
# and evaluate to Nil (they must NOT throw X::NoDispatcher). Only a routine
# that is not a multi at all -- a plain `sub`, or the mainline -- throws.
#
# mutsu used to push a multi-dispatch frame only when there was something to
# defer to, so a single-candidate multi established no dispatcher at all and
# every one of the four verbs died with
# "nextsame is not in the dynamic scope of a dispatcher".
# The Callable-value call path (`&e`, `my &c = &e; c()`) had the same gap for
# EVERY multi, one candidate or many, because it duplicated that guard.

plan 37;

# ---------------------------------------------------------------- the verbs

multi v-nextsame() { nextsame }
multi v-callsame() { callsame }
multi v-nextwith() { nextwith() }
multi v-callwith() { callwith() }

is v-nextsame().raku, 'Nil', 'nextsame in the only candidate returns Nil';
is v-callsame().raku, 'Nil', 'callsame in the only candidate returns Nil';
is v-nextwith().raku, 'Nil', 'nextwith in the only candidate returns Nil';
is v-callwith().raku, 'Nil', 'callwith in the only candidate returns Nil';

lives-ok { v-nextsame() }, 'nextsame in the only candidate lives';
lives-ok { v-callsame() }, 'callsame in the only candidate lives';
lives-ok { v-nextwith() }, 'nextwith in the only candidate lives';
lives-ok { v-callwith() }, 'callwith in the only candidate lives';

# ----------------------------------------------- every call position, by value

multi solo() { nextsame }

sub w-tail(&c)     { c() }
sub w-sink(&c)     { c(); 'lived' }
sub w-assign(&c)   { my $r = c(); 'lived' }
sub w-try-block(&c) { my $ok = try { c(); 1 }; $ok ?? 'lived' !! 'died' }
sub w-try-expr(&c) { try c(); 'lived' }
sub w-bare(&c)     { { c() }; 'lived' }
sub w-do(&c)       { do { c() }; 'lived' }
sub w-nested(&c)   { my $inner = sub { c() }; $inner(); 'lived' }

is w-tail(&solo).raku, 'Nil',
    'tail position: nextsame through an &-param yields Nil';
is w-sink(&solo), 'lived',
    'sink position: nextsame through an &-param does not throw';
is w-assign(&solo), 'lived',
    'assigned: nextsame through an &-param does not throw';
is w-try-block(&solo), 'lived',
    'inside try {}: nextsame through an &-param does not throw';
is w-try-expr(&solo), 'lived',
    'inside a try expression: nextsame through an &-param does not throw';
is w-bare(&solo), 'lived',
    'inside a bare block: nextsame through an &-param does not throw';
is w-do(&solo), 'lived',
    'inside a do block: nextsame through an &-param does not throw';
is w-nested(&solo), 'lived',
    'inside a nested anonymous sub: nextsame through an &-param does not throw';

# The by-name control: same routine, no Callable value in the way.
sub w-by-name() { solo(); 'lived' }
is w-by-name(), 'lived', 'by-name control: calling the multi directly lives';
is solo().raku, 'Nil', 'by-name control: the only candidate yields Nil';

# Bound to a scalar/`&` variable and invoked from there.
my &bound = &solo;
is bound().raku, 'Nil', 'invoking the multi through a bound & variable yields Nil';
my $stored = &solo;
is $stored().raku, 'Nil', 'invoking the multi through a scalar yields Nil';

# ------------------------------------------- redispatch still actually happens

multi chain(Int $x) { 'int(' ~ (callsame() // 'Nil') ~ ')' }
multi chain(Cool $x) { 'cool' }
is chain(1), 'int(cool)', 'callsame still redispatches to the next candidate';

multi chain2(Int $x) { 'int(' ~ (nextsame() // 'Nil') ~ ')' }
multi chain2(Cool $x) { 'cool' }
is chain2(1), 'cool', 'nextsame still tail-calls the next candidate';

multi chainw(Int $x) { 'int(' ~ (callwith('s') // 'Nil') ~ ')' }
multi chainw(Cool $x) { 'cool:' ~ $x }
is chainw(1), 'int(cool:s)', 'callwith still redispatches with new args';

# The LAST of several candidates has no next one either.
multi last-of-two(Int $x) { 'int' }
multi last-of-two(Str $x) { nextsame }
is last-of-two('s').raku, 'Nil', 'nextsame in the last of two candidates yields Nil';
is last-of-two(1), 'int', 'the other candidate of that multi still dispatches';

# Through a Callable value, with several candidates.
sub apply(&c, $arg) { my $r = c($arg); 'lived' }
is apply(&last-of-two, 'x'), 'lived',
    'a multi-candidate multi called through an &-param does not throw either';

# --------------------------------------------------------------------- methods

class OnlyMulti {
    multi method solo() { callsame }
}
is OnlyMulti.solo.raku, 'Nil', 'callsame in a single-candidate multi method yields Nil';
lives-ok { OnlyMulti.solo }, 'callsame in a single-candidate multi method lives';

class PlainOnly {
    method solo() { nextsame }
}
is PlainOnly.solo.raku, 'Nil', 'nextsame in a method with no parent method yields Nil';

class Base { method greet() { 'base' } }
class Derived is Base { method greet() { 'derived-' ~ callsame() } }
is Derived.greet, 'derived-base', 'callsame in a method still walks the MRO';

class MethodValue {
    multi method solo() { nextsame }
}
my $mv = MethodValue.new;
is $mv.solo.raku, 'Nil', 'a single-candidate multi method on an instance yields Nil';

# ------------------------------------- a plain sub / the mainline still throws

# NB: these deliberately use a mainline `try` rather than `dies-ok`/`throws-like`.
# `nextsame` looks for a dispatcher in the *dynamic* scope, and the Test routine
# that would invoke the block is itself dispatch-y, so wrapping them in a Test
# routine changes what they find (verified against rakudo). The roast file
# `S06-multi/redispatch.t` states the mainline case the same way.
sub plain-sub() { nextsame }
{
    try { plain-sub() };
    isa-ok $!, X::NoDispatcher,
        'nextsame in a plain (non-multi) sub throws X::NoDispatcher';
}

sub plain-sub-cs() { callsame }
{
    try { plain-sub-cs() };
    isa-ok $!, X::NoDispatcher,
        'callsame in a plain (non-multi) sub throws X::NoDispatcher';
}

sub plain-through-value(&c) { c(); 'lived' }
{
    try { plain-through-value(&plain-sub) };
    isa-ok $!, X::NoDispatcher,
        'a plain sub called through an &-param throws X::NoDispatcher';
}

{
    try { nextsame };
    isa-ok $!, X::NoDispatcher, 'nextsame in the mainline throws X::NoDispatcher';
}

# ------------------------------------------------ lastcall / nextcallee shapes

multi solo-lastcall() { lastcall; 'after' }
is solo-lastcall(), 'after', 'lastcall in the only candidate is a no-op that lives';

multi solo-nextcallee() { (nextcallee() // 'Nil').raku }
is solo-nextcallee(), '"Nil"', 'nextcallee in the only candidate yields Nil';

# done
