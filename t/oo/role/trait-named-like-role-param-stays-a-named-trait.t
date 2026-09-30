use Test;

# XML::Class: a parametric role's `Str :$xe` parameter binds a type object under
# the bare key `xe`; an attribute trait `is xe` in an unrelated class must still
# dispatch as a named trait rather than as a positional type.
plan 3;

role R[Str :$xe] {
    multi sub trait_mod:<is>(Attribute $a, Bool :$xe!) is export { $a.^name; }
    multi sub trait_mod:<is>(Attribute $a, Str:D :$xe!) is export { $a.^name; }
}
class Pre does R { has Int $.y; }

lives-ok { EVAL 'class I0 { has Int $.x is xe; }' }, "bare trait after composing the role";
lives-ok { EVAL 'class I1 { has Int $.x is xe("a"); }' }, "trait with an argument";
ok Pre.new(y => 1).y == 1, "composed class still works";
