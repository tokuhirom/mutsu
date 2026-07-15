use v6;
use Test;

# Pin for per-param `is copy` / `is rw` traits on MULTI-param pointy-block for
# loops. The compiler used to read traits only from the single-param def, so
# `-> $x is copy, $fn` marked BOTH params readonly and the first write died
# with X::Assignment::RO (this aborted roast/integration/precompiled.t, whose
# Test::Compile chains compunits with exactly this loop shape).

plan 8;

# is copy on the first of two params: writable, no writeback
{
    my @a = <a b c d>;
    my @seen;
    for @a -> $x is copy, $y {
        $x ~= "!";
        @seen.push("$x$y");
    }
    is @seen.join(","), "a!b,c!d", 'is copy param is assignable in a 2-param for';
    is @a.join(","), "a,b,c,d", 'is copy does not write back to the source';
}

# is copy actually executes the R~= shape used by roast Test::Compile
{
    my $last = '';
    my @out;
    for flat <a b> Z <f1 f2> -> $code is copy, $fn {
        $code [R~]= "use $last;\n" if $last;
        @out.push($code.lines.elems);
        $last = $fn;
    }
    is @out.join(","), "1,2", 'R~= onto an is-copy param works across iterations';
}

# is rw on one param of two: writes back only through that param
{
    my @a = 1, 2, 3, 4;
    for @a -> $x is rw, $y { $x += 10 }
    is @a.join(","), "11,2,13,4", 'is rw param in a 2-param for writes back';
}

# <-> makes all params rw
{
    my @a = 1, 2, 3, 4;
    for @a <-> $x, $y { $x += 10; $y += 20 }
    is @a.join(","), "11,22,13,24", '<-> two-param loop writes back both';
}

# plain multi-params stay readonly
{
    my $died = False;
    for <a b> -> $x, $y {
        $x = "z";
        CATCH { default { $died = True } }
    }
    ok $died, 'plain multi-param is still readonly';
}

# a plain sibling of an is-copy param stays readonly
{
    my $died = False;
    for <a b> -> $x is copy, $y {
        $y = "z";
        CATCH { default { $died = True } }
    }
    ok $died, 'plain sibling of an is-copy param is still readonly';
}

# single-param is copy unchanged
{
    my @a = <a b>;
    my @seen;
    for @a -> $x is copy { $x ~= "!"; @seen.push($x) }
    is @seen.join(","), "a!,b!", 'single-param is copy still works';
}

done-testing;
