use v6;
use Test;

# A ternary `?? !!` is an lvalue: an assignment (simple or compound) binds
# LOOSER than the conditional, so it applies to whichever branch is selected.
# `cond ?? A !! B op= rhs` parses as `(cond ?? A !! B) op= rhs`. Regression pin
# for the P5reset dist, whose reset() loop uses this shape.

# --- compound assignment through a ternary lvalue (unparenthesized) ---
{
    my $l;
    False ?? 9 !! $l ~= "x";
    is $l, "x", 'False ?? 9 !! $l ~= "x" appends to the selected else branch';
}
{
    my $a = "p";
    my $b = "q";
    False ?? $a !! $b ~= "x";
    is "$a|$b", "p|qx", 'compound assign hits the else branch when cond is false';
}
{
    my $a = "p";
    my $b = "q";
    True ?? $a !! $b ~= "x";
    is "$a|$b", "px|q", 'compound assign hits the then branch when cond is true';
}
{
    my $a = 1;
    my $b = 2;
    False ?? $a !! $b += 5;
    is "$a|$b", "1|7", 'numeric compound assign through a ternary lvalue';
}

# --- explicit-paren form ---
{
    my $a = "p";
    my $b = "q";
    (False ?? $a !! $b) ~= "x";
    is "$a|$b", "p|qx", 'parenthesized ternary lvalue compound assign';
}

# --- simple assignment through a ternary lvalue with a non-lvalue branch ---
{
    my $l;
    False ?? 9 !! $l = "x";
    is $l, "x", 'simple assign works when the taken branch is an lvalue (other branch a literal)';
}

# --- a non-lvalue branch, when SELECTED, is a runtime error ---
{
    my $l;
    dies-ok { True ?? 9 !! $l ~= "x" },
        'assigning through a literal branch when it is selected dies';
}

# --- statement-modifier conditional as a compound-assign lvalue ---
# `(EXPR if COND) op= rhs`: applies only when COND holds; a no-op otherwise.
{
    my $s = "p";
    ($s = "a" if 1) ~= "y";
    is $s, "ay", 'compound assign through a taken (if 1) conditional-assignment lvalue';
}
{
    my $s = "p";
    ($s = "a" if 0) ~= "y";
    is $s, "p", 'unrun (if 0) conditional-assignment lvalue is a silent no-op';
}

# --- the P5reset comb-loop shape end to end ---
{
    my $start;
    sub start-from() { my $value = $start; $start = Nil; $value }
    my $letters;
    for "xyz".comb -> $letter {
        $letter eq '-'
          ?? ($start = $letters.chop if $letters)
          !! $letters ~= $start
            ?? (start-from() .. $letter).join
            !! $letter;
    }
    is $letters, "xyz", 'P5reset comb-loop nested ternary-lvalue compound assign';
}

done-testing;
