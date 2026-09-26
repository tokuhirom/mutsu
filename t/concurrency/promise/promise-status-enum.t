use Test;

# `Promise.status` answers a real `PromiseStatus` enum value (Planned 0,
# Kept 1, Broken 2), not a bare Str, so it compares numerically, smartmatches
# the enum and reports the enum type -- the way rakudo does.

plan 12;

my $p = Promise.new;
isa-ok $p.status, PromiseStatus, 'status is a PromiseStatus';
ok $p.status == Planned, 'Planned == Planned';
nok $p.status == Kept, 'Planned != Kept';
$p.keep(42);
ok $p.status == Kept, 'kept promise status == Kept';
is $p.status.raku, 'PromiseStatus::Kept', '.raku names the enum';
is ~$p.status, 'Kept', 'stringifies to the key';
is +$p.status, 1, 'numifies to the value';
ok $p.status ~~ Kept, 'smartmatches the enum value';
my $seen = '';
given $p.status { when Kept { $seen = 'kept' } }
is $seen, 'kept', 'given/when on the status';
is Promise.broken("x").status, Broken, 'broken promise status';
is-deeply PromiseStatus.enums, Map.new((Planned => 0, Kept => 1, Broken => 2)),
    'PromiseStatus.enums';
ok $p.Capture<status> eqv PromiseStatus::Kept, 'Capture carries the enum value';
