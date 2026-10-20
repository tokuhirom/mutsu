use Test;
use MONKEY-SEE-NO-EVAL;
plan 15;

my %arguments;
multi trait_mod:<is>(Attribute $attribute, :$index!) {
    %arguments{$attribute.name} = $index;
}
sub next-index() { state $calls = 0; ++$calls }

class BracketTraits {
    has $.empty is index[];
    has $.single is index[1];
    has $.many is index[2, 3, 4];
    has $.nested is index[[5], [6, 7]];
    has $.quoted is index[']', '[', 'a]b'];
    has $.expression is index[next-index() + 10];
    has $.paren is index(8, 9);
    has $.paren-array is index([10, 11]);
    has @.typed is Array[Int];
}

isa-ok %arguments{'$!empty'}, Array, 'empty brackets pass an Array';
is %arguments{'$!empty'}.elems, 0, 'the empty array is not a bare true trait';
isa-ok %arguments{'$!single'}, Array, 'one bracketed value remains itemized';
is-deeply %arguments{'$!single'}, [1], 'the single value survives';
is-deeply %arguments{'$!many'}, [2, 3, 4], 'all bracketed values survive';
is-deeply %arguments{'$!nested'}, [[5], [6, 7]], 'nested arrays retain their shape';
is-deeply %arguments{'$!quoted'}, [']', '[', 'a]b'], 'quoted brackets do not end the argument';
is-deeply %arguments{'$!expression'}, [11], 'expressions execute inside bracket arguments';
is next-index(), 2, 'the trait expression executes once';
isa-ok %arguments{'$!paren'}, List, 'parenthesized multiple values remain a List';
is-deeply %arguments{'$!paren'}.List, (8, 9), 'parenthesized values retain their order';
isa-ok %arguments{'$!paren-array'}, Array, 'an explicitly parenthesized array remains an Array';
is-deeply %arguments{'$!paren-array'}, [10, 11], 'the parenthesized array is preserved';
is BracketTraits.new.typed.of, Int, 'known container type parameterization keeps its meaning';
dies-ok { EVAL('multi trait_mod:<is>(Attribute $a, :$index!) {}; class Spaced { has $.x is index [1] }') },
    'separated brackets do not become a trait argument';
