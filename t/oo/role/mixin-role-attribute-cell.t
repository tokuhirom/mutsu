use Test;

# A role mixed onto a non-Instance value owns a live attribute cell. Its
# construction markers are seeds only: repeated compiled method calls must see
# the previous call's scalar, array, and hash writes.

role Counter {
    has $!n = 0;
    method bump() { $!n = $!n + 1 }
    method n() { $!n }
}

sub check-counter($value, $name) {
    $value.bump;
    $value.bump;
    is $value.n, 2, "$name role attribute persists across calls";
}

check-counter({} but Counter, 'Hash');
check-counter([] but Counter, 'Array');
check-counter(0 but Counter, 'native scalar');

class Plain { }
check-counter(Plain.new but Counter, 'Instance mixin');

role Aggregate {
    has @.items;
    has %.counts;
    method add($value) {
        @!items.push($value);
        %.counts{$value} = %.counts{$value} + 1;
    }
}
my $aggregate = [] but Aggregate;
$aggregate.add('a');
$aggregate.add('a');
$aggregate.add('b');
is-deeply $aggregate.items, ['a', 'a', 'b'], 'role array attribute retains aggregate writes';
is $aggregate.counts<a>, 2, 'role hash attribute retains repeated writes';
is $aggregate.counts<b>, 1, 'role hash attribute retains a second key';

class HasCount {
    has $!n = 10;
    method bump() { $!n = $!n + 1 }
    method n() { $!n }
}
role HasCountRole {
    has $!n = 0;
    method role-bump() { $!n = $!n + 1 }
    method role-n() { $!n }
    method bump() { $!n = $!n + 1; nextsame }
}
my $separate = HasCount.new but HasCountRole;
$separate.role-bump;
$separate.role-bump;
$separate.bump;
is $separate.role-n, 3, 'role and class same-named attributes are independent';
is $separate.n, 11, 'nextsame writes the wrapped class attribute';

role First {
    has $!x = 1;
    method first() { $!x = $!x + 1; $!x }
}
role Second {
    has $!x = 10;
    method second() { $!x = $!x + 1; $!x }
}
my $same-named = 0 but First but Second;
is $same-named.first, 2, 'first role has an owner-qualified attribute cell';
is $same-named.second, 11, 'second role has an owner-qualified attribute cell';
is $same-named.first, 3, 'first same-named role state persists';
is $same-named.second, 12, 'second same-named role state persists';

role PublicCounter {
    has $.value is rw = 3;
    method set() { $!value = 7 }
}
my $public = 0 but PublicCounter;
$public.set;
is $public.value, 7, 'public role accessor reads the live role cell';
$public.value = 9;
is $public.value, 9, 'public role accessor assignment updates the live role cell';

my $qualified = 0 but Counter;
$qualified.Counter::bump;
$qualified.Counter::bump;
is $qualified.Counter::n, 2, 'qualified role dispatch keeps the role attribute cell';

role CloneCounter {
    has $!n = 0;
    method bump() { ++$!n }
    method n() { $!n }
}
my $original = 0 but CloneCounter;
$original.bump;
my $copy = $original.clone;
$original.bump;
is $original.n, 2, 'original mixin keeps its role state after clone';
is $copy.n, 1, 'Raku clone gets an independent role attribute cell';

done-testing;
