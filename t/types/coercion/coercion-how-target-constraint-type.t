use Test;

plan 7;

my \t = Int(Str);
is t.^target_type.^name, 'Int', '^target_type of Int(Str)';
is t.^constraint_type.^name, 'Str', '^constraint_type of Int(Str)';
is t.^nominal_target.^name, 'Int', '^nominal_target of Int(Str)';

my \u = Int();
is u.^target_type.^name, 'Int', '^target_type of Int()';
is u.^constraint_type.^name, 'Any', '^constraint_type of Int() is Any';

is Date(Str).^target_type.^name, 'Date', '^target_type of Date(Str)';
ok t.HOW.^name.ends-with('::CoercionHOW'), 'the HOW is a CoercionHOW';
