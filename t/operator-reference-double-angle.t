use Test;

plan 5;

my &eq = &infix:<<(==)>>;
ok eq(42, 42), '&infix:<<(==)>> returns a callable infix operator';
ok !eq(42, 41), '&infix:<<(==)>> callable preserves infix semantics';

# `<<...>>` / `«...»` / `[...]` interpolate/evaluate their content at
# runtime, unlike the literal `<...>` form -- the enclosed variable/
# expression names the operator dynamically. roast:
# S06-operator-overloading/infix.t.
{
    BEGIN my $plus = '+';
    is &infix:<<$plus>>(3, 4), 7, '&infix:<<$var>> resolves the operator name at runtime';
    is &infix:«$plus»(3, 4), 7, '&infix:«$var» resolves the operator name at runtime';
    is &infix:[$plus](3, 4), 7, '&infix:[$var] resolves the operator name at runtime';
}
