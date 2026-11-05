use Test;

# `@a «=« RHS`: the right operand sets the length of the hyper assignment, and
# the assignment writes only the elements it reaches (#15527). The expected
# values are rakudo's.

plan 7;

{
    my @a = 1, 2, 3;
    @a «=« (9, 8);
    is @a.join(","), "9,8,3", "a shorter right side keeps the unreached left elements";
}

{
    my @a = 1, 2, 3;
    @a «=» (9, 8);
    is @a.join(","), "9,8,9", "the right-dwim spelling still extends the right side";
}

{
    my @a = 1, 2, 3;
    @a «+=« (10, 20);
    is @a.join(","), "11,22,3", "a compound op keeps the unreached left elements";
}

{
    my @a = 1, 2;
    @a «=« (7, 8, 9);
    is @a.join(","), "9,8", "a longer right side cycles over the left and keeps its length";
}

{
    my @a = 1, 2, 3;
    @a «=« (9, 8, 7, 6);
    is @a.join(","), "6,8,7", "a right side longer than the array wraps around";
}

{
    my @a = 1, 2, 3;
    my @r = (@a «=« (9, 8));
    is @a.join(","), "9,8,3", "the array keeps the merged elements as an expression";
    is @r.join(","), "9,8", "the expression yields the hyper result";
}

done-testing;
