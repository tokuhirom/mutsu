use Test;
plan 6;

ok 1 === 1, 'Int identity equality';
ok "hello" === "hello", 'Str identity equality';
ok !(1 === 2), 'Int identity inequality';
ok !(1 === "1"), 'different types are not identical';
ok True === True, 'Bool identity equality';
ok Nil === Nil, 'Nil identity equality';
