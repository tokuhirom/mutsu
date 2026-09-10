use Test;

# Inline `where` constraints on `my` variable declarations are enforced both
# on initialization and on subsequent assignment, and `++`/`--` re-check the
# declared subset/type constraint.

plan 20;

# --- Block constraint -------------------------------------------------------
{
    my $a where { $_ > 10 } = 50;
    is $a, 50, 'block where: valid initializer accepted';
    throws-like { my $b where { $_ > 10 } = 5 },
        X::TypeCheck::Assignment, 'block where: invalid initializer dies';

    my $c where { $_ %% 2 } = 4;
    throws-like { $c = 7 }, X::TypeCheck::Assignment,
        'block where: invalid reassignment dies';
    is $c, 4, 'block where: value preserved after failed assignment';
    lives-ok { $c = 8 }, 'block where: valid reassignment lives';
    is $c, 8, 'block where: valid reassignment took effect';
}

# --- Whatever-code constraint ----------------------------------------------
{
    my $a where * > 100 = 200;
    is $a, 200, 'whatever where: valid initializer accepted';
    throws-like { my $b where * > 100 = 5 },
        X::TypeCheck::Assignment, 'whatever where: invalid initializer dies';
}

# --- Constraint combined with a declared type ------------------------------
{
    my Int $a where * %% 3 = 9;
    is $a, 9, 'typed where: valid initializer accepted';
    throws-like { my Int $b where * %% 3 = 10 },
        X::TypeCheck::Assignment, 'typed where: failing predicate dies';
    throws-like { my Int $c where * %% 3 = "x" },
        X::TypeCheck::Assignment, 'typed where: failing base type dies';
}

# --- Code-variable constraint ----------------------------------------------
{
    my &even = { $_ %% 2 };
    my $a where &even = 6;
    is $a, 6, 'code-var where: valid initializer accepted';
    throws-like { my $b where &even = 7 },
        X::TypeCheck::Assignment, 'code-var where: invalid initializer dies';
}

# --- ++/-- re-check a named subset constraint ------------------------------
{
    subset Even of Int where { $_ % 2 == 0 };
    my Even $x = 2;
    throws-like { $x++ }, X::TypeCheck::Assignment, 'subset var cannot be ++ed';
    is $x, 2, 'value preserved after failed ++';
    throws-like { $x-- }, X::TypeCheck::Assignment, 'subset var cannot be --ed';
    is $x, 2, 'value preserved after failed --';
}

# --- ++ that lands on a valid value still works ----------------------------
{
    subset BigEnough of Int where { $_ >= 0 };
    my BigEnough $n = 5;
    lives-ok { $n++ }, 'in-range ++ lives';
    is $n, 6, 'in-range ++ updates the value';
    throws-like { $n = -1 }, X::TypeCheck::Assignment,
        'out-of-range assignment still dies';
}
