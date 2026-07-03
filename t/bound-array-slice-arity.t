use Test;

# `my @slice := @array[1,2]` binds @slice to a fixed-arity view over
# @array's elements 1 and 2. A later whole-array assignment writes through
# those elements, bounded by @slice's OWN (fixed) length — extra RHS values
# are discarded and missing ones pad with Any (roast
# S09-subscript/slice.t test 15).

plan 8;

{
    my @array = <a b c d>;
    my @slice := @array[1,2];
    is ~(@slice = <A B C D>), "A B",
        'bound slice assignment discards extra RHS values';
    is-deeply @array, ["a", "A", "B", "d"],
        'bound slice assignment writes through to the source array';
}

{
    my @array = <a b c d>;
    my @slice := @array[1,2];
    is ~(@slice = ("Z",)), "Z ",
        'bound slice assignment with fewer RHS values pads with Any';
    is @array[2].WHAT, Any, 'missing bound slice element becomes Any';
}

{
    # identity: the bound slice's elements ARE @array's elements, and stay
    # bound even after a plain (non-`:=`) whole-array reassignment.
    my @array = <a b c d>;
    my @slice := @array[1,2];
    ok @array[1] =:= @slice[0], 'bound slice element shares identity with source';
    @slice = <A B C D>;
    @array[1] = "Z";
    is @slice[0], "Z", 'bound slice stays bound after a whole-array reassignment';
}

{
    # a single-index bind to a container-valued leaf writes through too.
    my @array = 1, 2, [3, 4];
    my @x := @array[2];
    @x = 5, 6;
    is-deeply @array, [1, 2, [5, 6]],
        'single-index container-valued-leaf bind writes through';
}

{
    # ordinary whole-container bind (not an index/slice) is unaffected.
    my @array = 1, 2, 3;
    my @b := @array;
    @b = 9, 8, 7;
    is-deeply @array, [9, 8, 7], 'plain whole-container bind still works';
}
