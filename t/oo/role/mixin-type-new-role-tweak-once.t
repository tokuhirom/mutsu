use Test;
plan 9;

my $base-tweaks = 0;
my $role-builds = 0;
my $role-tweaks = 0;
class MixinTypeBase {
    has $.value = 42;
    submethod TWEAK { $base-tweaks++ }
}
role MixinTypeRole {
    submethod BUILD { $role-builds++ }
    submethod TWEAK { $role-tweaks++ }
}
role MixinTypeOtherRole { method other { True } }

my $mixed = MixinTypeBase but MixinTypeRole;
is $base-tweaks, 0, 'composing a type object does not construct the base';
is $role-builds, 0, 'composing a type object does not run role BUILD';
is $role-tweaks, 0, 'composing a type object does not run role TWEAK';

my $instance = $mixed.new;
is $instance.value, 42, 'the mixed type constructs its base attributes';
is $base-tweaks, 1, 'the base TWEAK runs once';
is $role-builds, 1, 'the role BUILD runs once during construction';
is $role-tweaks, 1, 'the role TWEAK runs once during construction';

my $grouped = MixinTypeBase but (MixinTypeRole, MixinTypeOtherRole);
is $role-tweaks, 1, 'composing a group of roles does not run TWEAK';
$grouped.new;
is $role-tweaks, 2, 'grouped role TWEAK runs once during construction';
