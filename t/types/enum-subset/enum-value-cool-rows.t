use Test;

# ADR-11276 §9.147: an enum value is a value of its base type, whose MRO
# reaches `Cool`: a method the enumeration roles and the base type do not
# declare is `Cool`'s row (`a.sign` numifies the value, a `Str`-valued
# value's `sign` fails to numify, as in Rakudo).

plan 6;

enum Signs (neg => -3, zero => 0, pos => 5);
is neg.sign, -1, 'Int-valued enum sign, negative';
is zero.sign, 0, 'Int-valued enum sign, zero';
is pos.sign, 1, 'Int-valued enum sign, positive';
is neg.abs, 3, 'abs of an Int-valued enum';

enum Words (word => "abc", num => "42");
throws-like { word.sign }, X::Str::Numeric, 'a Str-valued enum value numifies its value';
is num.sign, 1, 'a numeric Str-valued enum value has a sign';
