use Test;

# Code.Capture and Code.clone are rows of the method table (ADR-12523, slice 5).

plan 8;

sub f($a) { 42 }
my &g = -> $x { };

throws-like { &f.Capture }, X::Cannot::Capture, 'a sub cannot be unpacked';
throws-like { &g.Capture }, X::Cannot::Capture, 'a pointy block cannot';
throws-like { { $_ }.Capture }, X::Cannot::Capture, 'a bare block cannot';
throws-like { /a/.Capture }, X::Cannot::Capture, 'a regex cannot';
throws-like { &say.Capture }, X::Cannot::Capture, 'a builtin routine cannot';

is &f.clone.name, 'f', 'a cloned sub keeps its name';
is (&f.clone)(1), 42, 'a cloned sub still runs';
ok /a/.clone ~~ Regex, 'a regex clones instead of dying';
