use v6;
use Test;

plan 9;

# `my @array does R1` applies the role to the container as a compile-time
# declaration effect, so a following BEGIN block (which runs before the block's
# runtime statements) still sees the mixed-in role. Previously the BEGIN was
# hoisted above the `does` mixin, so `@array.test` threw at compile time and
# aborted the whole program.
{
    role R1 { method test { 42 } }
    {
        my @array does R1;
        is @array.test, 42, 'role mixed in at point of declaration (runtime)';

        my $x;
        BEGIN { $x = @array.test }
        is $x, 42, 'declaration mixin visible to a following BEGIN block';
    }
}

# `.^name` of a role-mixed value carries a `+{Role,...}` suffix.
{
    role Bar { }
    is (5 but Bar).^name, 'Int+{Bar}', '.^name of a simple role mixin';

    role Foo::Bar { }
    is (5 but Foo::Bar).^name, 'Int+{Foo::Bar}',
        '.^name keeps the full role name from a deeper namespace';

    my $x = 5;
    $x does Bar;
    is $x.^name, 'Int+{Bar}', '.^name after `does` on a scalar';
}

# Allomorphic mixins (`but True`, `<5>`) are unaffected by the role suffix.
is (0 but True).^name, 'Int', '.^name of a value mixin is not role-suffixed';
is <5>.^name, 'IntStr', '.^name of an allomorph is unchanged';

# Indexing a role-mixed positional value delegates to the inner container when
# the role supplies no AT-POS of its own.
is ((^Inf) but role {})[2], 2, 'index into a role-mixed lazy range';
is ([10, 20, 30] but role {})[1], 20, 'index into a role-mixed array';
