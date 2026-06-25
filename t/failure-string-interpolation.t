use v6;
use Test;

# A Failure is an "unthrown exception" that explodes when used as a value.
# Interpolating an unhandled Failure into a string must throw its underlying
# exception (like prefix:<~> / .Str), not silently render "Failure()".

plan 6;

# Interpolating an unhandled Failure throws.
{
    my $q = "x";
    my $f = $q<foo>;   # associative index on a Str -> a Failure
    dies-ok({ my $s = "val=$f end" }, 'interpolating an unhandled Failure throws');
}

# The thrown exception carries the right message.
{
    my $q = "x";
    my $f = $q<foo>;
    throws-like({ my $s = "v=$f" }, Exception,
                message => /'does not support associative indexing'/,
                'interpolation throws the underlying exception message');
}

# Handling the Failure first (// ) means no throw, and the default is used.
{
    my $q = "x";
    my $f = $q<foo>;
    my $v = $f // 'DEFAULT';
    is $v, 'DEFAULT', 'a // -handled Failure does not throw';
    lives-ok({ my $s = "v=$v" }, 'interpolating the handled result does not throw');
}

# Smartmatch / .defined handle the Failure (no throw).
{
    my $q = "x";
    my $f = $q<foo>;
    ok ($f ~~ Failure), 'Failure smartmatches Failure (handles it)';
    nok $f.defined, 'an (now handled) Failure is not defined';
}
