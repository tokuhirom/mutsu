use Test;

plan 2;

# A typed `my TYPE $name;` declaration seeds env[name] with the TYPE object at
# hoist time; that placeholder must not make a same-named attribute TRAIT look
# like a type, which turned the trait's named-arg dispatch into a
# positional-type dispatch and killed the multi (JSON::Marshal t/140-opt-in.t:
# `my Str $json;` in scope broke every `is json` attribute).
multi sub trait_mod:<is> (Attribute $a, :$mark!) { }

class T {
    has Str $.a is mark;
}
pass 'class with a custom named attribute trait registered';

my Str $mark;
class T2 {
    has Str $.b is mark;
}
pass '... even with a same-named typed variable in scope';

done-testing;
