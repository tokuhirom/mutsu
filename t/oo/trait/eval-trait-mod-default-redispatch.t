use Test;

plan 3;

# A custom attribute trait that re-dispatches to CORE's
# `trait_mod:<is>(Attribute, :$default!)` works in a unit EVAL'd from a string
# held in a variable, and from its RakuAST tree, not only in the main program.
my $src = Q[multi trait_mod:<is>(Attribute $attr, :$dv!) {
    trait_mod:<is>($attr, :default($dv));
}
class RedispatchBox { has Str $.m is dv("Hi"); }
RedispatchBox.new.m];
my $copy = $src ~ "";

is EVAL($copy), 'Hi', 'a string EVAL applies the re-dispatched default';

my $other = $src.subst('RedispatchBox', 'RedispatchBox2', :g);
use experimental :rakuast;
is EVAL($other.AST), 'Hi', 'EVAL of the tree applies it too';
is EVAL($src.subst('RedispatchBox', 'RedispatchBox3', :g).subst('Hi', 'Yo')), 'Yo', 'the default tracks the trait argument';
