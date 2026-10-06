use Test;

# From the Injector distribution: a role stubbing `method package {...}` is
# mixed into an Attribute, which already has `package`; the stub only
# requires it and must not shadow the native method.
plan 3;

role R { method package {...}; method hi { self.package.^name } }
class C { has $.x; }
my $a = C.^attributes[0];

lives-ok { $a does R }, 'role with a stub for a native Attribute method mixes in';
is $a.package.^name, 'C', 'the stub does not shadow the native method';
is $a.hi, 'C', 'role method reaches the native method';
