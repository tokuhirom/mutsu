use Test;

# #9046: an X/Z meta-assignment whose lvalue is a literal list of scalars
# parsed in the bracketed spelling (`($a, $b) X[+=] 2, 3`) but not the
# unbracketed one (`($a, $b) X+= 2, 3`), which was a syntax error. Raku makes
# no distinction between the two.
#
# The variable-lvalue spellings were fixed in #9033, but those go through
# `assign_stmt` / `try_assign`, which parse a lexical variable name before
# they look at the operator -- a parenthesized list never reaches either. The
# list-infix loop, which is where a literal-list lvalue does land, only
# handled `R` from `parse_meta_compound_assign_op` and let `X`/`Z` fall
# through to a scan that took `X+` and choked on the stranded `=`.
#
# Each unbracketed assertion below is paired with its bracketed twin, since
# the point is that the two spellings mean the same thing.

plan 12;

# --- Cross: each left container accumulates every right element. ---
{
    my ($a, $b) = 1, 10;
    ($a, $b) X+= 2, 3;
    is "$a,$b", '6,15', 'unbracketed X+= writes back to each scalar in a literal list';
}
{
    my ($a, $b) = 1, 10;
    ($a, $b) X[+=] 2, 3;
    is "$a,$b", '6,15', 'the bracketed twin still agrees';
}
{
    my ($a, $b) = 1, 10;
    my @r = ($a, $b) X+= 2, 3;
    is-deeply @r, [3, 6, 12, 15], 'unbracketed X+= evaluates to every intermediate value';
    is "$a,$b", '6,15', 'unbracketed X+= still writes back when its value is captured';
}
{
    my ($a, $b) = 1, 2;
    ($a, $b) X~= "x", "y";
    is "$a,$b", '1xy,2xy', 'unbracketed X~= concatenates every right element in turn';
}

# --- Zip: element-wise. ---
{
    my ($a, $b) = 1, 2;
    ($a, $b) Z+= 10, 20;
    is "$a,$b", '11,22', 'unbracketed Z+= assigns element-wise into a literal list';
}
{
    my ($a, $b) = 1, 2;
    ($a, $b) Z[+=] 10, 20;
    is "$a,$b", '11,22', 'the bracketed twin still agrees';
}
{
    my ($a, $b);
    my @r = ($a, $b) Z+= 1, 2;
    is-deeply @r, [1, 2], 'unbracketed Z+= evaluates to the assigned values';
    is "$a,$b", '1,2', 'unbracketed Z+= mutates both containers';
}
{
    my ($a, $b) = 2, 3;
    ($a, $b) Z*= 10, 100;
    is "$a,$b", '20,300', 'unbracketed Z*= multiplies element-wise';
}

# --- The variable-lvalue spellings (#9033) must be untouched by this: they
# are handled before the list-infix loop and must not be swallowed by it. ---
{
    my @a = 1, 2;
    @a X+= (10, 20);
    is-deeply @a, [31, 32], 'an @-variable lvalue still accumulates in place';
}
{
    my $s = 1;
    $s X+= (10, 20);
    is $s, 31, 'a $-variable lvalue still folds every right element in';
}
