use v6;
use Test;

# `Enumeration` is not only the constraint every enum value satisfies -- it is a
# real composable core role with state (`has $.key`, `has $.value`) that an
# ordinary class may compose to get a key/value pair with the enum API on top.
# `raku-doc/doc/Type/Enumeration.rakudoc` documents exactly that with its
# `class DNA does Enumeration` example, and the `Logic::Ternary` distribution
# does the same. mutsu had `Enumeration` as a type-check constraint only, so
# `class Foo does Enumeration` died as `X::InvalidType: Invalid typename
# 'Enumeration'` (#8115).
#
# Every expectation below was measured against rakudo. The methods that reach
# `self.^enum_values` (`enums`, `pred`, `succ`, `pick`, `roll`, `CALL-ME`) are
# deliberately NOT provided: a `ClassHOW` has no `enum_values`, so they die in
# rakudo too, and faking them would be a divergence.

plan 18;

class Tern does Enumeration {
    method new(Str:D $val) { self.bless: key => $val, value => 1 }
}

my $t = Tern.new('yes');

# The role's state, reachable through the accessors it generates on the
# composing class.
is $t.key,   'yes', 'the composed role supplies a $.key accessor';
is $t.value, 1,     'the composed role supplies a $.value accessor';

# Role membership, from all three directions.
ok $t ~~ Enumeration,      'an instance of the composing class ~~ Enumeration';
ok $t.does(Enumeration),   '.does(Enumeration) on the instance';
ok Tern ~~ Enumeration,    'the composing class type object ~~ Enumeration';
is Tern.^roles.map(*.^name).join(','), 'Enumeration',
    '.^roles lists the composed Enumeration role';

# The derived API the role supplies on top of the two attributes.
is $t.kv.join(' '), 'yes 1', '.kv is (key, value)';
is $t.pair.gist,    'yes => 1', '.pair is key => value';
is $t.Numeric,      1, '.Numeric is the value';
is $t.Int,          1, '.Int is the value as an Int';
is $t.Real,         1, '.Real is the value';
is $t.gist,         'yes', '.gist is the key';
is $t.raku,         'Tern::yes', '.raku is Name::key';

# An `Enumeration` parameter binds an instance of the composing class, just as
# it binds an enum value.
{
    sub takes-enumeration(Enumeration $x) { $x.key }
    is takes-enumeration($t), 'yes', 'an Enumeration parameter binds the instance';
}

# The documented example: the composing class reads the role's attribute as
# `$!key` from inside its own method, and may override `gist`.
#
# That override is also the pin on the role's methods being `multi`s with an
# explicit `::?CLASS:D:` invocant, as rakudo's are (they arrive as candidates on
# `Mu`'s/`Any`'s dispatchers, not as `only` methods of the role). Declared
# `only`, rakudo rejects the class's `multi method gist` outright ("Cannot have
# a multi candidate for 'gist' when an only method is also in the package"), and
# mutsu reports `Ambiguous call to 'gist(DNA: )'` -- which is what this assertion
# caught when #8119 stopped a class multi from displacing a role candidate with
# the same invocant smiley.
{
    class DNA does Enumeration {
        my %pairings = %( A => 'T', T => 'A', C => 'G', G => 'C' );
        method new($base-pair where 'A' | 'C' | 'G' | 'T') {
            self.bless(key => $base-pair, value => %pairings{$base-pair});
        }
        multi method gist(::?CLASS:D:) { "$!key -> $!value" }
    }
    my $b = DNA.new('A');
    is $b.value, 'T', "the class's own new() blesses the role's attributes";
    is $b.gist, 'A -> T',
        'the class reads the role attributes as $!key/$!value and overrides gist';
}

# Composing the role does not disturb enum values, which are their own `Value`
# shape and satisfy `Enumeration` natively rather than through a role list.
{
    enum E <a b>;
    ok (a ~~ Enumeration), 'an enum value still ~~ Enumeration';
    nok (42 ~~ Enumeration), 'a non-composing, non-enum value still does not';
}

done-testing;
