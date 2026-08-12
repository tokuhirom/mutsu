use v6;
use Test;

plan 8;

# The RHS of the `s{pattern} = expr` assignment-replacement form sits at
# item-assignment precedence: the loose word-logical operators (and/or/...)
# bind looser, so they belong to the enclosing statement, not the replacement.
# Regression: Text::CSV's `$fragment ~~ s:i{^ "row="} = "" and self.rowrange
# ($fragment)` parsed `"" and self.rowrange(...)` as the replacement, so
# rowrange was never invoked (short-circuited inside the replacement closure).

{
    my $t = "row=2-*";
    my $called = 0;
    sub f() { $called = 1; True }
    $t ~~ s{^ "row\=" } = "" and f();
    is $t, "2-*", "substitution applied";
    is $called, 1, "'and f()' ran as part of the enclosing statement";
}

{
    my $t = "row=2-*";
    my $called = 0;
    sub g() { $called = 1; True }
    # No match: `and` must not run.
    $t ~~ s{^ "col\=" } = "" and g();
    is $t, "row=2-*", "no substitution on non-match";
    is $called, 0, "'and' RHS skipped when the match failed";
}

# `or` binds looser too.
{
    my $t = "abc";
    my $called = 0;
    sub h() { $called = 1; True }
    $t ~~ s{x} = "y" or h();
    is $called, 1, "'or' ran after a failed substitution";
}

# The replacement itself still takes a full item-assignment-tight expression.
{
    my $t = "a1b";
    $t ~~ s{\d} = "<" ~ "X" ~ ">";
    is $t, "a<X>b", "compound tight expression stays the replacement";
}

# Non-destructive S[] = expr form (operates on the topic).
{
    my $called = 0;
    sub k() { $called = 1; True }
    given "row=9" {
        my $r = (S{^ "row\=" } = "") and k();
        is $r, "9", "S/// assignment form returned the substituted copy";
    }
    is $called, 1, "'and' after S/// assignment form ran (not swallowed)";
}

done-testing;
