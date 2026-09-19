use Test;

# A type's `.HOW` must be identity-stable: rakudo mints one metaclass
# instance per type and hands out the SAME object on every access, so a
# mutation applied directly to it (`does`, or an attribute set straight on
# the instance) is visible on the next access. mutsu used to construct a
# brand-new `Instance` value on every single `.HOW` call, so any mutation
# applied to the returned value was lost as soon as it went out of scope.
# https://github.com/tokuhirom/mutsu/issues/8791

plan 9;

class Foo {}
is Foo.HOW.WHICH, Foo.HOW.WHICH, 'repeated .HOW access on a class returns the identical metaobject';

my $obj = Foo.new;
is $obj.HOW.WHICH, Foo.HOW.WHICH, 'an instance and its type object share the same metaobject';

is 1.HOW.WHICH, 2.HOW.WHICH, 'two Int values share the identical Int metaobject';
is 1.HOW.WHICH, Int.HOW.WHICH, 'an Int value and the Int type object share the identical metaobject';

my role R { method mooish { "role-method" } }

class Bar {}
nok Bar.HOW ~~ R, 'Bar does not do R before mixing it into its HOW';
Bar.HOW does R;
ok Bar.HOW ~~ R, 'does mixed directly into a HOW persists on the very next access';
ok Bar.HOW ~~ R, 'and keeps persisting on a further access';

my $bar = Bar.new;
ok $bar.HOW ~~ R, 'an instance sees the mixin applied to its type object HOW';

# Roles keep their own distinct metaobject identities even though a punned
# instance and its role candidates share the role's bare display name with
# the role GROUP -- these must not collide in the metaobject cache.
my role Punned { method m { 42 } }
my $punned = Punned.new;
is $punned.HOW.^name, 'Perl6::Metamodel::ClassHOW',
    'a punned role instance keeps an ordinary ClassHOW, not the role group HOW';
