use Test;

plan 5;

# A role body runs after the class's parents are known and before the role's
# own methods are composed into the class (#11628).

class Base { method greet { "base" } }
role R {
    my $mro = ::?CLASS.^mro.map(*.^name).join(',');
    my &orig := ::?CLASS.^find_method('greet');
    method greet { "role+" ~ orig(self) }
    method mro-seen { $mro }
}
class C is Base does R { }

is C.new.greet, 'role+base', 'find_method in a role body finds the parent method';
is C.new.mro-seen, 'C,Base,Any,Mu', 'the role body sees the class MRO with its parents';

# Without a parent override the role's method is still composed.
role Plain { method hi { "hi" } }
class P does Plain { }
is P.new.hi, 'hi', 'a role without a body still composes its methods';

# The role body does not see the role's own methods in the class yet.
role Own {
    my $seen = ::?CLASS.^find_method('own') ?? 'found' !! 'missing';
    method own { $seen }
}
class O does Own { }
is O.new.own, 'missing', "the role's own method is not composed before its body runs";

# Methods of a role closing over the body's lexicals still work.
role Counter {
    my $n = 41;
    method next { ++$n }
}
class K does Counter { }
is K.new.next, 42, 'composed method keeps the role body lexical capture';
