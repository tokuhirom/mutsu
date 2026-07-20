use Test;

# `//` (JumpIfNotNil) must honor a user-defined `.defined` method, not just
# the structural definedness check. A role-composed mixin (`but role {...}`)
# and a class instance can both override `.defined`; `//` (and its JIT-compiled
# form) previously read through the wrapper and kept the left value.

plan 8;

# --- Mixin (`but role {...}`) with a `.defined` override -------------------
{
    my Int $i = 1 but role { method defined { False } };
    is $i.defined, False, 'mixin role .defined override returns False';
    is $i // "x", "x", '`//` uses the mixin role .defined override (undefined -> fallback)';
}

{
    my Int $i = 5 but role { method defined { True } };
    is $i // "x", 5, '`//` keeps the value when the override reports defined';
}

# --- Class instance with a `.defined` override ----------------------------
{
    class C { method defined { False } }
    my $c = C.new;
    is $c.defined, False, 'instance .defined override returns False';
    is $c // "y", "y", '`//` uses the instance .defined override (undefined -> fallback)';
}

# --- Plain values keep their ordinary `//` semantics ----------------------
{
    my $undef;
    is $undef // "d", "d", 'undefined scalar falls back';
    is 5 // "e", 5, 'defined scalar keeps its value';
    my Int $typed;
    is $typed // "f", "f", 'uninitialized typed scalar falls back';
}
