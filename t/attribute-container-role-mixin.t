use Test;

plan 7;

# A user trait_mod that mixes a parameterized role into the attribute's
# *container* (`$attr.container.VAR does Role(arg)`) must reach the per-instance
# container: scalar via `.VAR`, array/hash directly. (roast S14-traits/attributes.t)

role doc { has $.doc is rw }
multi trait_mod:<is>(Attribute $a, doc, $arg) {
    $a.container.VAR does doc($arg);
}

class T {
    has $.dog is doc('barks');
    has @.birds is doc('tweet');
    has %.cows is doc('moooo');
}

my $x = T.new;
ok $x ~~ T, 'instance constructed with container-mixin attributes';
is $x.dog.VAR.doc, 'barks', 'scalar attribute container carries role state via .VAR';
is $x.birds.doc, 'tweet', 'array attribute container carries role state';
is $x.cows.doc, 'moooo', 'hash attribute container carries role state';

# The trait state is per-attribute, distinct for each.
isnt $x.birds.doc, $x.cows.doc, 'each attribute keeps its own mixed-in state';

# A second instance sees the same composed container state.
my $y = T.new;
is $y.dog.VAR.doc, 'barks', 'second instance inherits scalar container mixin';
is $y.cows.doc, 'moooo', 'second instance inherits hash container mixin';
