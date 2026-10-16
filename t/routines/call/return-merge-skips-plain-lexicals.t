use Test;

# A call's return no longer copies the callee's binding of any plain user
# lexical back into the caller (ADR-12529 §3, slice 37). Every write a callee
# makes to an outer lexical reaches the declaring frame some other way: the
# variable's shared cell, or the runtime-named write carrier. This file pins
# that across the named, light and typed light call paths, and that a caller's
# own same-named lexical is never the target. Checked against rakudo.

plan 8;

sub run(&c) { c() }

my $s = 0;
sub plain-w() { $s++ }
sub typed-w(Int $n) { $s += $n }
plain-w();
typed-w(10);
is $s, 11, 'an outer scalar written by an untyped and a typed sub';

my @a;
my %h;
sub fill(Str $k) { @a.push($k); %h{$k} = 1 }
fill('x');
is @a.join(','), 'x', 'an outer array pushed by a sub';
is %h.keys.join(','), 'x', 'an outer hash element stored by a sub';

my $c = 0;
run { $c = 5 };
is $c, 5, 'a closure run by another routine';

my $shadow = 'outer';
sub w() { $shadow = 'written' }
sub keeps-own() { my $shadow = 'mine'; w(); $shadow }
is keeps-own(), 'mine', "a caller's same-named lexical is not written";
is $shadow, 'written', 'the declaration-site variable is';

our $g = 1;
sub set-g($n) { $::($n) = 5 }
set-g('g');
is $g, 5, 'a runtime-named write';

sub rec(Int $n) { $s++; rec($n - 1) if $n > 0 }
rec(3);
is $s, 15, 'a recursive sub writing an outer scalar at every depth';
