use Test;

# An INIT or CHECK phaser that reads or writes a lexical of the routine it is
# written in sees that lexical's static state, and what it stores there is what
# every frame of the routine starts from (#10562). Each expectation below is
# what rakudo prints.

plan 33;

sub f { my $z; INIT $z = 5; $z }
is f(), 5, 'an INIT write to a routine lexical is the value the routine starts from';

class C { method m { my $z; INIT $z = 6; $z } }
is C.m, 6, 'the same in a method of a class';

role R { method m { my $z; INIT $z = 7; $z } }
class UR does R { }
is UR.m, 7, 'the same in a method of a role';

sub with-init { my $z = 3; INIT $z = 5; $z }
is with-init(), 3, 'the lexical\'s own initializer still runs on each call';

sub chk { my $z; CHECK $z = 8; $z }
is chk(), 8, 'a CHECK write is the value the routine starts from';

sub arr { my @a; INIT @a.push(1); @a }
is-deeply arr(), [1], 'an array lexical keeps what an INIT pushed';

sub hsh { my %h; INIT %h<a> = 1; %h }
is-deeply hsh(), {a => 1}, 'a hash lexical keeps what an INIT stored';

sub twice { my $z; INIT $z = 1; INIT $z++; $z }
is twice(), 2, 'several INIT phasers of one routine run in source order';

sub order { my @l; INIT @l.push(1); INIT @l.push(2); CHECK @l.push(0); @l }
is-deeply order(), [0, 1, 2], 'CHECK runs before INIT';

sub value-form { my $z; my $x = INIT $z = 9; $z ~ '/' ~ $x }
is value-form(), '9/9', 'a value-form INIT stores the lexical and yields its value';

my $anon = sub { my $z; INIT $z = 4; $z };
is $anon(), 4, 'the same in an anonymous sub';

my $pointy = -> $a { my $z; INIT $z = 16; $z + $a };
is $pointy(1), 17, 'the same in a pointy block';

sub nested-block { my $r; if True { my $z; INIT $z = 2; $r = $z }; $r }
is nested-block(), 2, 'the same in a block nested in a routine';

my @loop;
for 1..2 { my $z; INIT $z = 6; @loop.push: $z }
is-deeply @loop, [6, 6], 'every iteration of a loop body starts from the static value';

sub typed { my Int $z; INIT $z = 12; $z }
is typed(), 12, 'a typed lexical';

sub via-routine { my $z; sub helper { $z = 13 }; INIT helper(); $z }
is via-routine(), 13, 'an INIT that calls a routine of the scope that writes the lexical';

sub both { my $z; BEGIN $z = 1; INIT $z++; $z }
is both(), 2, 'a BEGIN and an INIT share the lexical\'s static cell';

sub param($x) { my $z; INIT $z = $x.defined; $z }
is param(5), False, 'a parameter is unbound when the INIT runs';

class Shared { my $s = 10; method m { my $z; INIT $z = $s; $z } }
is-deeply Shared.m, Any, 'the INIT sees a class lexical in its static state too';

class Attr { has $.x = 3; method m { my $z; INIT $z = 11; $z + $!x } }
is Attr.new.m, 14, 'a method that also reads an attribute keeps working';

class E is export { method m { my $z; INIT $z = 8; $z } }
is E.m, 8, 'an exported class';

class Outer {
    class Inner { method m { my $z; INIT $z = 9; $z } }
    method i { Inner.m }
}
is Outer.i, 9, 'a class nested in a class';

is EVAL('sub ev { my $z; INIT $z = 14; $z }; ev()'), 14, 'in an EVAL';

my @order;
sub o1 { my $z; INIT @order.push('o1'); INIT $z = 1; $z }
sub o2 { my $z; INIT @order.push('o2'); INIT $z = 2; $z }
is-deeply @order, ['o1', 'o2'], 'the phasers of different routines run in source order';

{
    my $z;
    INIT $z = 5;
    is $z, 5, 'a block lexical of the unit keeps what an INIT stored';
}

# A phaser that ends a routine is the routine's value (#10644).

sub tail-init { INIT 5 }
is tail-init(), 5, 'a routine that ends in an INIT returns its value';

sub tail-check { CHECK 6 }
is tail-check(), 6, 'the same for a CHECK';

class TailC {
    method m { INIT 7 }
    method n { my @a = INIT (4, 5); @a.elems }
}
is TailC.m, 7, 'a method that ends in an INIT';
is TailC.n, 2, 'a value-form INIT list flattens into an array';

role TailR { method m { INIT 8 } }
class TailUR does TailR { }
is TailUR.m, 8, 'a role method that ends in an INIT';

sub tail-after-decl { my $x = 1; INIT 9 }
is tail-after-decl(), 9, 'an INIT after other statements';

sub tail-list { INIT (1, 2, 3) }
is-deeply tail-list(), (1, 2, 3), 'a list value stays a list';

sub not-tail { INIT 10; 11 }
is not-tail(), 11, 'an INIT that does not end the routine is not its value';
