use Test;

# #10858: `LEXICAL::` is every lexical visible from the current scope, not
# just the current frame (that is `MY::`). Enclosing blocks' variables, the
# file scope's, and an enclosing routine's are all reachable; an inner
# declaration shadows an outer one of the same name.

plan 11;

my $top = 0;

{
    my $x = 1;
    my $y = 2;
    {
        my $x = 10;
        is LEXICAL::<$x>, 10, 'the innermost declaration shadows an outer one';
        is LEXICAL::<$y>, 2, "an enclosing block's variable is visible";
        is LEXICAL::<$top>, 0, "the file scope's variable is visible";
        is-deeply LEXICAL::<$nope>, Nil, 'an undeclared name is Nil';
        is OUTER::LEXICAL::<$x>, 1, 'OUTER::LEXICAL:: starts one frame out';
        is-deeply MY::<$y>, Nil, 'MY:: still holds only the current frame';
        $y = 3;
        is LEXICAL::<$y>, 3, 'a later write to the outer variable is seen';
    }
}

sub in-routine {
    my $in-sub = 5;
    {
        is LEXICAL::<$in-sub>, 5, "a nested block sees its routine's variable";
        is LEXICAL::<$top>, 0, 'and the file scope across the routine boundary';
    }
}
in-routine;

my &closure = {
    my $z = 7;
    -> { is LEXICAL::<$z>, 7, "a pointy block sees its closure's variable" }();
};
closure();

sub outer-sub { 1 }
{
    is LEXICAL::<&outer-sub>.name, 'outer-sub', 'an enclosing routine is listed';
}
