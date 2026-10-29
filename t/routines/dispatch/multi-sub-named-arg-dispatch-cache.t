use Test;

# A multi sub called with named arguments is resolved from a cache keyed by the
# positional argument types plus each named argument's name and value type.
# Every shape is called several times so the later calls are cache hits; each
# must keep picking the candidate the first one did.

plan 20;

multi sub pick(Int $x, Bool :$flag!) { "int-flag" }
multi sub pick(Int $x, Int :$count!) { "int-count" }
multi sub pick(Int $x, *%rest) { "int-rest:" ~ %rest.keys.sort.join(',') }
multi sub pick(Str $s, :$flag!) { "str-flag" }
multi sub pick(Str $s) { "str-plain" }

multi sub def(Int:D :$v!) { "defined" }
multi sub def(Int:U :$v!) { "undefined" }
multi sub def(:$other!) { "other" }

my $t = True;
my $n = 3;

for 1..3 {
    is pick(1, :flag($t)), 'int-flag', "Bool named picks the Bool candidate (round $_)";
}
for 1..3 {
    is pick(1, :count($n)), 'int-count', "Int named picks the Int candidate (round $_)";
}
for 1..3 {
    is pick(1, :other($n)), 'int-rest:other', "an unknown name falls to the slurpy hash (round $_)";
}
is pick(1, :other(1), :more(2)), 'int-rest:more,other', 'two named arguments';
is pick('s', :flag($t)), 'str-flag', 'a Str positional with a named argument';
is pick('s'), 'str-plain', 'no named argument still resolves on its own';

for 1..2 {
    is def(:v(5)), 'defined', "a defined Int named value (round $_)";
    is def(:v(Int)), 'undefined', "an Int type object as the named value (round $_)";
}
is def(:other(1)), 'other', 'a different name selects the other candidate';
is pick(2, :flag(False)), 'int-flag', 'the same winner for the other Bool value';

# A named argument whose value only a `where` candidate accepts is not cached by type.
multi sub w(Int :$v! where * > 0) { "positive" }
multi sub w(Int :$v!) { "other" }
is w(:v(5)), 'positive', 'a where candidate sees the first value';
is w(:v(-5)), 'other', 'and the next value, with the same type key';
