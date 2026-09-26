use Test;

# `self.bless` inside a user `method new` invoked on a mixin type object
# (`(Base but R).new(...)`) blesses the base class and composes the mixed-in
# roles, exactly like the default `.new` does. It used to die with "bless can
# only be called on a class or instance" -- CSS::Properties::Util's
# `(Color but CSS::Units[Colors, 'rgba']).new(...)` constant.

plan 4;

class C {
    has $.r;
    proto method new(|) { * }
    multi method new(:$r!, *%c) { self.bless: :$r, |%c }
}
role R[\dimension, \units] { method type { units } }
my enum Colors « :rgb :rgba »;

my $o = (C but R[Colors, 'rgba']).new(:r(3));
is $o.r, 3, 'the base attributes are set through bless';
is $o.type, 'rgba', 'the mixed-in role is composed';
ok $o ~~ R, 'the instance does the role';

role S { method s { 's' } }
class D { has $.x; method new(*%c) { self.bless(|%c) } }
is (D but S).new(:x(1)).s, 's', 'a plain custom new works too';
