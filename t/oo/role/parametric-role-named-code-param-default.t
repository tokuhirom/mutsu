use Test;

# Issue #9391: a parameterized role's named `&`-parameter with a default
# (`:&cmp = &infix:<cmp>`) must bind that default in the role body when the
# role is composed without the argument. Reduced from
# Concurrent::PriorityQueue 0.0.2, whose role forwards `:&cmp` to
# Array::Sorted::Util's `inserts`.
plan 9;

multi sub forward(\list, Int:D \key, :&cmp!, :$force!) {
    cmp(key, 1);
}

role Ordered[::TYPE = Any, :&cmp = &infix:<cmp>] {
    method has-cmp()    { &cmp.defined }
    method via(Int:D \k) { forward [], k, :&cmp, :force }
    method type-name()  { TYPE.^name }
}

class Bare does Ordered { }
ok Bare.new.has-cmp, 'omitted named & parameter binds its default';
is Bare.new.via(3), More, 'defaulted &cmp forwards as a named code argument';
is Bare.new.type-name, 'Any', 'defaulted type capture alongside it';

class Typed does Ordered[Int] { }
is Typed.new.via(0), Less, 'default still binds when a positional is supplied';
is Typed.new.type-name, 'Int', 'supplied positional type capture';

class Reversed does Ordered[Int, :cmp(-> $a, $b { $b cmp $a })] { }
is Reversed.new.via(3), Less, 'an explicit :cmp overrides the default';
is Reversed.new.type-name, 'Int', 'type capture with an explicit :cmp';

my $punned = Ordered.new;
ok $punned.has-cmp, 'punned role binds the default too';
is $punned.via(2), More, 'punned role forwards the default &cmp';
