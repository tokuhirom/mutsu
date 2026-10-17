use Test;

# ADR-11276 §9.145: `Mu.set_why` is a row of `Mu`, reached for any invocant
# (`t/types/mu-set-why-on-instance.t` pins that it documents the invocant's
# type).

plan 4;

my $x = 42;
is $x.set_why("forty-two"), "forty-two", 'set_why answers its argument';
is $x.WHY, "forty-two", '.WHY reads it back';
is "text".set_why("on a Str"), "on a Str", 'a Str invocant';

ok Mu.^can('set_why'), 'Mu.^can("set_why")';
