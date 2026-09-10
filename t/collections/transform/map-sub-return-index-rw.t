use v6;
use Test;

# 99problems-21-to-30.t fixes (P23/P26/P22-range):
# - `return` in a `sub` callback invoked via map ends that call (routine
#   semantics), instead of escaping as X::ControlFlow::Return.
# - Recursive calls passing a slice of an immutable List param no longer
#   corrupt the call-site index-rw writeback temps (which fired a bogus
#   `@xs[1..*] = ...` on the immutable List).
# - The (<=) subset operator enumerates a Range operand.

plan 8;

{
    my $f = sub ($x) { return $x * 2; };
    is-deeply (map $f, (1, 2, 3)).List, (2, 4, 6), 'return in sub callback via listop map';
    is-deeply (1, 2, 3).map($f).List, (2, 4, 6), 'return in sub callback via .map';

    my $compress = sub ($x) {
        state $previous = '';
        return $x ne $previous ?? ($previous = $x) !! ();
    }
    my @r = map $compress, <a b c>;
    is @r.elems, 3, 'state + conditional return in sub callback (P23 shape)';
}

{
    sub g(@xs) {
        if @xs == 0 { () }
        else { @xs[0], g(@xs[1..*]).Slip }
    }
    is-deeply g((3, 4, 5)).List, (3, 4, 5),
        'recursion over immutable List slices does not fire a bogus writeback';

    sub combination($n, @xs) {
        if $n > @xs { () }
        elsif $n == 0 { ([],) }
        elsif $n == @xs { ([@xs],) }
        else {
            combination($n-1, @xs[1..*]).map({ [@xs[0], |$_ ] }).Slip,
                combination($n, @xs[1..*]).Slip;
        }
    }
    is combination(2, (1..3)).elems, 3, 'P26 combination over a Range-derived List';
}

{
    # A genuine `is rw` mutation through an index argument still writes back.
    sub bump($x is rw) { $x = 99 }
    my @a = 1, 2, 3;
    bump(@a[0]);
    is-deeply @a, [99, 2, 3], 'is rw index-arg writeback still works';
}

{
    my @n = (1..49).pick(6);
    ok ?(@n (<=) (1..49)), '(<=) enumerates a Range RHS';
    ok !(?((0, 1) (<=) (1..49))), '(<=) with Range RHS rejects out-of-range elements';
}
