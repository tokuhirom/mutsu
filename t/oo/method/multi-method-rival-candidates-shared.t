use Test;

plan 12;

# The visible multi candidates that do not bind a call still shape rakudo's
# tiers (#12795). They are shared with the dispatch, not copied; the answers
# must not depend on how many of them there are or on which owner holds them.

role Shape {
    multi method kind(Int $x) { 'role-int' }
    multi method kind(Str $x) { 'role-str' }
}

class Base {
    multi method kind(Int $x where * > 100) { 'base-big' }
    multi method kind(Int $x) { 'base-int' }
    multi method kind(Rat $x) { 'base-rat' }
    multi method kind(Num $x) { 'base-num' }
    multi method kind(Bool $x) { 'base-bool' }
    multi method kind(List $x) { 'base-list' }
    multi method kind(Hash $x) { 'base-hash' }
    multi method kind(Range $x) { 'base-range' }
    multi method kind(Pair $x) { 'base-pair' }
    multi method kind(Set $x) { 'base-set' }
    multi method kind(Bag $x) { 'base-bag' }
    multi method kind(Seq $x) { 'base-seq' }
    multi method kind(Map $x) { 'base-map' }
    multi method kind(Code $x) { 'base-code' }
    multi method kind(Blob $x) { 'base-blob' }
    multi method kind(Complex $x) { 'base-complex' }
    multi method kind(Mu $x) { 'base-mu' }
}

class Derived is Base {
    multi method kind(Int $x where * < 0) { 'derived-neg' }
    multi method kind(Str $x) { 'derived-str' }
}

class WithRole is Base does Shape { }

my $b = Base.new;
my $d = Derived.new;
my $r = WithRole.new;

is $b.kind(5), 'base-int', 'the narrower of the unbound-rival families wins';
is $b.kind(500), 'base-big', 'a where-constrained candidate outranks the plain one';
is $b.kind(1.5), 'base-rat', 'a different type binds its own candidate';
is $b.kind(True), 'base-bool', 'Bool, one of many rivals';
is $b.kind(<a b>), 'base-list', 'List';
is $b.kind(Any), 'base-mu', 'falls to Mu when nothing narrower binds';

is $d.kind(-3), 'derived-neg', 'a subclass candidate beats the ancestor family';
is $d.kind(7), 'base-int', 'an ancestor candidate answers when the subclass has none';
is $d.kind('s'), 'derived-str', 'a subclass candidate for another type';

is $r.kind(5), 'role-int', 'a composed role candidate beats the ancestor class candidate';
is $r.kind('s'), 'role-str', 'a role candidate answers for a type the ancestors lack';

# Repeating the calls (the rank memo and the declared index are warm) agrees.
is ($b.kind(5), $d.kind(-3), $r.kind('s')).join(','), 'base-int,derived-neg,role-str',
    'second round';
