use Test;

# Code.signature is a row of the method table (ADR-12523, slice 3).

plan 8;

sub f(Int $a, $b?, :$c --> Str) { }
sub s(*@r) { }
my &g = -> $x, $y { };
class C { method m($x, $y?) { } }

is &f.signature.raku, ':(Int $a, $b?, :$c --> Str)', 'a declared sub';
is &s.signature.raku, ':(*@r)', 'a slurpy';
is &g.signature.raku, ':($x, $y)', 'a pointy block';
is { $_ }.signature.raku, ':(;; $_? is raw = OUTER::<$_>)', 'a block with the implicit topic';
is (-> { }).signature.raku, ':()', 'an empty pointy block';
is C.^lookup('m').signature.params.elems, 4, 'a method object counts its invocant and the implicit *%_';
is &f.signature.params.map(*.name).join(','), '$a,$b,$c', 'parameter names';
is &f.signature.returns.raku, 'Str', 'the declared return type';
