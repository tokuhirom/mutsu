use Test;

# The routine form of `orelse`, `andthen` and `notandthen` folds its operands
# as Rakudo's `infix:<orelse>(+a)` does: a Callable operand is called with the
# value so far (when it takes an argument), which is what makes
# `Failure.raku.EVAL` rebuild a handled Failure (#18603).

plan 12;

is &infix:<orelse>(Nil, *.self).^name, 'Nil', 'orelse calls *.self with the undefined value';
is &infix:<orelse>(Nil, { 42 }), 42, 'a block operand';
is &infix:<orelse>(Nil, -> $x { $x.^name }), 'Nil', 'a pointy block gets the value';
is &infix:<orelse>(Int, Str, *.^name), 'Str', 'three operands';
is &infix:<orelse>(5, *.self), 5, 'a defined value answers without calling';
is &infix:<andthen>(1, *+1), 2, 'andthen calls with the defined value';
is &infix:<andthen>(1, { $_ + 5 }), 6, 'andthen with a block';
is-deeply &infix:<andthen>(Nil, *+1), Empty, 'andthen stops at an undefined value';
is-deeply &infix:<andthen>(1, Nil, *+1), Empty, '... at any position';
is &infix:<notandthen>(Nil, *.^name), 'Nil', 'notandthen calls with the undefined value';
is-deeply &infix:<notandthen>(1, *.^name), Empty, 'notandthen stops at a defined value';

my $f = Failure.new("x");
$f.so;
is-deeply $f.raku.EVAL.handled, True, "a handled Failure's .raku.EVAL is handled";
