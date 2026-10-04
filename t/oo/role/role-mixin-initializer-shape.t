use Test;

# `$x does R(v)` / `$x but R(v)`: only a top-level call with exactly one
# argument (positional, or named with its name ignored) is a role initializer (Rakudo rewrites it to
# `infix:<does>($x, R, :value(v))` at compile time). Anything else is an
# ordinary call, and a role called outside that shape is a coercion.

plan 7;

role R { has $.x }

is (1 but R(5)).x, 5, 'but R(v) initializes the role';

{
    my $a = 1;
    $a does R(6);
    is $a.x, 6, 'does R(v) initializes the role';
}

{
    role Q { has $.y; method CALL-ME(|) { 'called' } }
    is (1 but Q(7)).y, 7, "the initializer form does not consult the role's CALL-ME";
}

is (1 but R(:y(9))).x, 9, 'a single named argument is the initializer, its name ignored';

throws-like { my $a = 1; $a does R(1, 2) }, X::Coerce::Impossible,
    'two arguments are an ordinary call, not an initializer';

throws-like { 1 but (R(8)) }, X::Coerce::Impossible,
    'a parenthesized call is an ordinary call, not an initializer';

# A role initializer whose argument dies must not leave anything behind that
# turns a later, ordinary role call into an initializer.
{
    my $a = 1;
    try { $a does R(die 'boom') };
    throws-like { R(5) }, X::Coerce::Impossible,
        'a role call after an aborted initializer is still a coercion';
}
