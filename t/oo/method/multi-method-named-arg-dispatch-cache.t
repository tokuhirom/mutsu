use Test;

# A multi method called with a named argument is resolved from a cache keyed by
# the receiver, the positional types, and each named argument's name and value
# type. Every shape is called three times so the second and third calls are
# served from the cache; each must keep picking the candidate the first did.

plan 18;

class K {
    multi method pick(Int $x, Bool :$flag!) { "int-flag" }
    multi method pick(Int $x, Int :$count!) { "int-count" }
    multi method pick(Int $x, *%rest) { "int-rest:" ~ %rest.keys.sort.join(',') }
    multi method pick(Str $s, :$flag!) { "str-any-flag" }
    multi method pick(Str $s) { "str-plain" }

    multi method def(Int:D :$v!) { "defined" }
    multi method def(Int:U :$v!) { "undefined" }
    multi method def(:$other!) { "other" }
}

my $k = K.new;
my $t = True;
my $n = 3;

for 1..3 {
    is $k.pick(1, :flag($t)), 'int-flag', "Bool named picks the Bool candidate (round $_)";
}
for 1..3 {
    is $k.pick(1, :count($n)), 'int-count', "Int named picks the Int candidate (round $_)";
}
for 1..3 {
    is $k.pick(1, :other($n)), 'int-rest:other', "an unknown name falls to the slurpy hash (round $_)";
}
is $k.pick(1, :other(1), :more(2)), 'int-rest:more,other', 'two named arguments';
is $k.pick('s', :flag($t)), 'str-any-flag', 'a Str positional with a named argument';
is $k.pick('s'), 'str-plain', 'no named argument still resolves on its own';

# Definedness of the named value is part of the key.
for 1..2 {
    is $k.def(:v(5)), 'defined', "a defined Int named value (round $_)";
    is $k.def(:v(Int)), 'undefined', "an Int type object as the named value (round $_)";
}
is $k.def(:other(1)), 'other', 'a different name selects the other candidate';
is $k.pick(2, :flag(False)), 'int-flag', 'the same winner for the other Bool value';
