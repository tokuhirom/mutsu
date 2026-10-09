use Test;

# Came from the XML::Class ecosystem distribution (Audio::Hydrogen's suite):
# `multi d(TypedNode $e, DeserialiseX $a, $obj)` against
# `multi d(ElementWrapper $e, ElementX $a, Str $obj)`. A subset's refinement
# must not make its candidate "narrower" on a parameter whose nominal type is
# unrelated to the other candidate's (a role): Rakudo ties that parameter, so
# the candidate with the narrower `Str` wins instead of the first declared.

plan 4;

class Node { }
role EW { }
role D { }
role E { }
my subset TN of Node;

multi sub e(TN $e, $obj) { 'A' }
multi sub e(EW $e, Str $obj) { 'B' }

my $x = Node.new;
$x does EW;
is e($x, Str), 'B', 'subset-typed candidate does not outrank an unrelated role by refinement';

multi sub f(TN $e, D $a) { 'A' }
multi sub f(EW $e, E $a) { 'B' }
my $at = "at";
$at does D;
$at does E;
is f($x, $at), 'A', 'fully tied candidates keep declaration order';

# The recursive shape that overflowed the stack before the fix.
multi sub d(TN $e, D $a, $obj) { d($e, $a, Str) }
multi sub d(EW $e, E $a, Str $obj) { 'done' }
is d($x, $at, Version), 'done', 'recursive re-dispatch reaches the narrower candidate';

# A refinement still wins when the nominal types are the same.
my subset Small of Int where * < 10;
multi sub g(Int $n) { 'Int' }
multi sub g(Small $n) { 'Small' }
is g(3), 'Small', 'same nominal type: the subset still out-narrows';
