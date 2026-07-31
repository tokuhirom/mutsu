use v6;
use Test;

plan 6;

# `has %.h is SomeType` builds the attribute as an instance of SomeType. The
# `dispatch_new` seeding did that, but the `bless` route seeded a plain Hash —
# DBIish's `has %.Converter is DBDish::TypeConverter` (whose Connection is
# constructed through bless) lost its STORE/convert-function surface and every
# fetched column value went through "No such method 'convert-function'".

role TC does Associative {
    has %!store handles <AT-KEY EXISTS-KEY>;

    method tag() { 'tc' }
    method STORE(::?CLASS:D: \to_store) {
        for @(to_store) {
            when Pair { %!store{.key} = .value }
        }
    }
}

class ViaBless {
    has %.conv is TC;
    has @.hooks is TC;

    method new() { self.bless }

    submethod BUILD() {
        %!conv = (a => 1, b => 2);
    }
}

my $c = ViaBless.new;
is $c.conv.^name, 'TC', 'a %-attr with `is Role` blessed as that type';
is $c.conv.tag, 'tc', 'its methods dispatch';
is $c.conv<a>, 1, 'BUILD assignment went through the role STORE';
is $c.conv<b>, 2, 'both pairs';
ok $c.conv<b>:exists, 'EXISTS-KEY delegates too';
is $c.hooks.^name, 'TC', 'an @-attr with `is Role` builds the same way';
