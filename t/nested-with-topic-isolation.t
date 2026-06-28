use Test;

# A nested `with`/`given` must not let the inner block's topic leak back into
# the outer block's topic-source variable. Regression for the `given $x { with
# EXPR {} }` clobber (roast S32-io/io-handle.t `.say method`): the inner `with`
# topicalized `$_` to EXPR's value, which was wrongly written back to `$x`
# through the outer `given`'s topic-source alias.

plan 9;

# Literal inner topic.
{
    my $x = "foos";
    with $x {
        with 12345 { }
        is $x, "foos", 'literal inner with does not clobber outer my var';
    }
}

# Non-literal (rvalue) inner topic.
{
    my $x = "outer";
    sub inner-val { "INNER" }
    with $x {
        with inner-val() { }
        is $x, "outer", 'rvalue inner with does not clobber outer my var';
    }
}

# Outer topic is a (read-only) sub parameter, exactly like the roast shape.
{
    sub check (Str :$nl-out) {
        with $nl-out {
            with 999 { }
            is $nl-out, "set", 'inner with does not clobber outer sub param (literal)';
            with "other".uc { }
            is $nl-out, "set", 'inner with does not clobber outer sub param (rvalue)';
        }
    }
    check(:nl-out("set"));
}

# given (not with) outer scope.
{
    my $g = 3;
    given $g {
        with 100 { }
        is $g, 3, 'inner with does not clobber outer given var';
    }
}

# Inner topic restored, outer `$_ = ...` still writes back to the source.
{
    my $v = "start";
    given $v {
        with 7 { }
        $_ = "changed";
    }
    is $v, "changed", 'outer given $_ writeback still works after inner with';
}

# `do with` value propagation is unaffected by the inner-topic routing.
{
    sub f { 42 }
    is (do with f() { $_ + 1 }), 43, 'do with rvalue returns block value';
    is (do with f() { $_ * 2 }), 84, 'do with rvalue returns last expr';
}

# Deeply nested with does not leak across two levels.
{
    my $a = "A";
    with $a {
        with "B" {
            with "C" { }
        }
        is $a, "A", 'triple-nested with leaves outermost var intact';
    }
}
