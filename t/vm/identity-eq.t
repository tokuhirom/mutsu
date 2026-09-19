use Test;
plan 12;

ok 1 === 1, 'Int identity equality';
ok "hello" === "hello", 'Str identity equality';
ok !(1 === 2), 'Int identity inequality';
ok !(1 === "1"), 'different types are not identical';
ok True === True, 'Bool identity equality';
ok Nil === Nil, 'Nil identity equality';
ok Date.new('2000-01-01') === Date.new('2000-01-01'),
    'equal Date values have the same identity';
ok !(Date.new('2000-01-01') === Date.new('2000-01-02')),
    'different Date values have different identities';
is Date.new('2000-01-01').WHICH.Str, 'Date|51544',
    'Date WHICH uses the Modified Julian daycount';
ok DateTime.new('2000-01-01T00:00:00Z') === DateTime.new('2000-01-01T00:00:00Z'),
    'equal DateTime values have the same identity';
ok !(DateTime.new('2000-01-01T00:00:00Z') === DateTime.new('2000-01-01T00:00:01Z')),
    'different DateTime values have different identities';
is DateTime.new('2000-01-01T00:00:00Z').WHICH.Str,
    'DateTime|2000-01-01T00:00:00Z',
    'DateTime WHICH uses the canonical timestamp';
