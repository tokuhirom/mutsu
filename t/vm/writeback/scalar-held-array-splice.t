use Test;

# `.splice` on a scalar variable holding a real array (`my $n = [...]`) must
# mutate it in place, like `@a.splice`. The plain scalar array-mutator fast path
# covers push/pop/shift/unshift/append/prepend but not splice, so a scalar-held
# splice previously left the array unchanged. This also covers `is Array`
# instances, whose splice delegates through a scalar-named temp binding.

plan 8;

# scalar holding an array
{
    my $n = [1, 2, 3, 4, 5];
    my @removed = $n.splice(1, 2);
    is $n.join(","), "1,4,5", 'scalar-held splice removes in place';
    is @removed.join(","), "2,3", 'splice returns the removed elements';
}

# splice with a replacement list
{
    my $n = [1, 2, 3, 4];
    my @removed = $n.splice(1, 2, (8, 9));
    is $n.join(","), "1,8,9,4", 'scalar-held splice inserts the replacement';
    is @removed.join(","), "2,3", 'splice returns removed when replacing';
}

# is-Array instance splice
{
    class S is Array {}
    my $s = S.new;
    $s.push(1, 2, 3, 4, 5);
    $s.splice(1, 2);
    is $s.Array.join(","), "1,4,5", 'is-Array instance splice mutates the instance';
}

# @-sigiled splice still works (no regression)
{
    my @a = 1, 2, 3, 4, 5;
    my @removed = @a.splice(1, 2);
    is @a.join(","), "1,4,5", '@-array splice still works';
    is @removed.join(","), "2,3", '@-array splice returns removed';
}

# a scalar bound to an array via := splices through the shared container
{
    my @a = 1, 2, 3, 4;
    my $r := @a;
    $r.splice(1, 1);
    is @a.join(","), "1,3,4", 'splice through a := bound scalar writes to the source';
}
