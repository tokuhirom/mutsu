use Test;

plan 2;

my &eq = &infix:<<(==)>>;
ok eq(42, 42), '&infix:<<(==)>> returns a callable infix operator';
ok !eq(42, 41), '&infix:<<(==)>> callable preserves infix semantics';
