use v6;
use Test;

plan 21;

# `anon class`/`anon role`/`anon grammar NAME` must keep the declared name on
# the type object (`.^name`, gist) while installing NO symbol anywhere — not
# a bareword, not a package-qualified stash entry, not even a self-reference
# from inside its own body. See `raku-doc/doc/Language/variables.rakudoc`,
# "The `anon` declarator". Two independent bugs, both from the SAME
# underlying leak (the package declarators never mangled their registry key
# the way `anon sub NAME`/`my class Foo {}` already do):
#
#   Repro 1: `Name` (or `::('Name')`) resolves to the anon type afterward.
#   Repro 2: two `anon class Name {}` declarations share ONE type object.
#
# raku raises a COMPILE-TIME "Undeclared name" for a bare `Name` reference
# after `anon class Name {}` (mutsu has no such compile-time check, so that
# half is not directly comparable). `::('Name')` is a RUNTIME indirect
# lookup instead, and both raku and mutsu agree it fails (`Failure`) — that
# is the oracle repro 1 uses below, so this file runs unchanged under both
# `raku` and mutsu.
#
# IMPORTANT: raku hoists every NAMED (non-anon) class/role/grammar
# declaration for compile-time symbol resolution across the WHOLE file,
# regardless of where it physically sits or whether it ever executes. A
# `::('Name')` runtime probe would therefore see a same-named REAL
# declaration ANYWHERE in this file, even one written far below. Every name
# probed via `::('...')` below is therefore unique to this file and never
# reused by a real (non-anon) declaration elsewhere in it.

# --- repro 1: no symbol installed anywhere (class/role/grammar) -----------

{
    my $a = anon class AnonLeakClass {};
    my $t = ::('AnonLeakClass');
    isa-ok $t, Failure, 'anon class installs no ::(\'Name\') symbol';
}

{
    my $a = anon role AnonLeakRole {};
    my $t = ::('AnonLeakRole');
    isa-ok $t, Failure, 'anon role installs no ::(\'Name\') symbol';
}

{
    my $a = anon grammar AnonLeakGrammar { token TOP { . } };
    my $t = ::('AnonLeakGrammar');
    isa-ok $t, Failure, 'anon grammar installs no ::(\'Name\') symbol';
}

# A named anon class also installs no symbol reachable through a nested
# package's own stash.
{
    package AnonLeakPkg {
        our $a = anon class AnonLeakQux {};
    }
    my $t = ::('AnonLeakPkg::AnonLeakQux');
    isa-ok $t, Failure, 'anon class inside a package installs no Pkg::Name symbol';
}

# --- repro 2: two declarations of the same name are two distinct types ----

{
    my $a = anon class AnonDupClass {};
    my $b = anon class AnonDupClass {};
    is $a === $b, False, 'two anon class Name {} declarations are distinct types';
    is $a.^name, 'AnonDupClass', 'first anon class keeps its display name';
    is $b.^name, 'AnonDupClass', 'second anon class keeps its display name too';
}

{
    my $a = anon role AnonDupRole {};
    my $b = anon role AnonDupRole {};
    is $a === $b, False, 'two anon role Name {} declarations are distinct types';
}

{
    my $a = anon grammar AnonDupGrammar { token TOP { . } };
    my $b = anon grammar AnonDupGrammar { token TOP { . } };
    is $a === $b, False, 'two anon grammar Name {} declarations are distinct types';
}

# The SAME declaration site re-executed (a loop body) keeps ONE identity —
# distinguishes "site-unique" mangling from "every execution is unique".
{
    my @seen;
    for 1..3 {
        @seen.push(anon class AnonLoopClass {});
    }
    is @seen[0] === @seen[1], True, 'the same anon class site reused in a loop keeps its identity (1)';
    is @seen[1] === @seen[2], True, 'the same anon class site reused in a loop keeps its identity (2)';
}

# --- surrounding semantics must keep working -------------------------------

{
    my $a = anon class AnonMethClass { has $.x = 5; method greet { "hi from " ~ self.^name } };
    my $o = $a.new;
    is $o.x, 5, 'a named anon class constructs via .new';
    is $o.^name, 'AnonMethClass', '.^name on an instance reports the declared name';
    is $o.greet, 'hi from AnonMethClass', 'self.^name inside a method sees the declared name';
}

{
    class AnonComposeBase { method hi { 'base hi' } }
    role AnonComposeRole { method r { 'role r' } }
    my $a = anon class AnonComposeClass is AnonComposeBase does AnonComposeRole {};
    is $a.new.hi, 'base hi', 'anon class is Parent inherits the named parent';
    is $a.new.r, 'role r', 'anon class does Role composes the named role';
}

# An anon class does NOT shadow/collide with an unrelated non-anon class of
# the same name declared in the same scope.
{
    class ShadowClash { method tag { 'named' } }
    my $a = anon class ShadowClash { method tag { 'anon' } };
    is ShadowClash.new.tag, 'named', 'the real (non-anon) class is untouched by the anon declaration';
    is $a.new.tag, 'anon', 'the anon instance uses its own methods';
    is $a === ShadowClash, False, 'the anon type object is distinct from the real one';
}

# gist/say render the declared name in parens, matching a normal type object.
{
    my $a = anon class AnonGistClass {};
    is $a.gist, '(AnonGistClass)', 'a named anon class gists with its name in parens';
}

# --- the already-fixed unnamed form must still work unchanged -------------

{
    my $a = anon class {};
    like $a.^name, /^ '<anon|' \d+ '>' $/, 'unnamed anon class still displays as <anon|N>';
}

done-testing;

# vim: ft=raku
