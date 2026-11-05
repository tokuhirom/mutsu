use v6;
use Test;

plan 6;

# From Hash::Util: a method object taken from a hash with `.can` BEFORE a role is
# mixed into it names the core type's own method, so it keeps running the native
# method after the role overrides it (the role's wrapper calls the saved original;
# calling the override from itself recursed to a stack overflow).
role R {
    method AT-KEY(\k) is raw { "role" }
    method EXISTS-KEY(\k) { "role-exists" }
}

my %h = q => 9;
my $at     = %h.can('AT-KEY').head;
my $exists = %h.can('EXISTS-KEY').head;
is $at(%h, 'q'), 9, 'before the mixin: the native AT-KEY';

%h does R;
is %h<q>, 'role', 'the role override is what ordinary dispatch finds';
is $at(%h, 'q'), 9, 'the saved native AT-KEY still reads the hash';
ok $exists(%h, 'q'), 'the saved native EXISTS-KEY finds the key';
nok $exists(%h, 'zz'), 'and not a missing one';

my %other = z => 3;
my $o2 = %other.can('AT-KEY').head;
my %mixed = c => 4;
%mixed does R;
is $o2(%mixed, 'c'), 4, 'a method taken from another hash runs natively on the mixed one';
