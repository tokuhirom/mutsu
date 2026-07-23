use Test;

# A `lazy gather {...}` Seq (or `gather {...}.lazy`) is lazy: count/numeric
# coercions (`.elems`, `.Int`, `.Numeric`) throw X::Cannot::Lazy (a soft
# Failure, recoverable with `//`). A plain `gather {...}` Seq is NOT lazy:
# `.elems` reifies and counts.
#
# Regression: mutsu exempted every gather-backed Seq from the X::Cannot::Lazy
# check via the `__mutsu_lazylist_from_gather` marker, so `lazy gather.elems`
# eagerly reified and returned a count instead of throwing. The `lazy` prefix
# (`gather.lazy`) is now distinguished by the preserve-lazy marker, and the VM
# force-reify path skips it for count/numeric coercions.

plan 10;

# --- plain gather: elems reifies and counts ---
is (gather { take 1; take 2 }).elems, 2, 'plain gather .elems reifies and counts';
is (gather { take $_ for 1..3 }).elems, 3, 'plain gather over a loop counts';

# --- lazy gather: elems/Int/Numeric throw X::Cannot::Lazy ---
{
    my $l = lazy gather { take 1; take 2 };
    is ((try $l.elems) // $!.^name), 'X::Cannot::Lazy', 'lazy gather .elems throws X::Cannot::Lazy (soft)';
}
{
    my $l = lazy gather { take 1; take 2 };
    is ((try $l.Int) // $!.^name), 'X::Cannot::Lazy', 'lazy gather .Int throws X::Cannot::Lazy (soft)';
}
{
    my $l = lazy gather { take 1; take 2 };
    is ((try $l.Numeric) // $!.^name), 'X::Cannot::Lazy', 'lazy gather .Numeric throws X::Cannot::Lazy (soft)';
}

# --- gather.lazy method form is the same ---
{
    my $l = (gather { take 1 }).lazy;
    is ((try $l.elems) // $!.^name), 'X::Cannot::Lazy', 'gather.lazy .elems throws X::Cannot::Lazy';
}

# --- a lazy gather assigned directly to @ stays lazy: .elems throws ---
{
    my @a = lazy gather { take 1; take 2 };
    is ((try @a.elems) // $!.^name), 'X::Cannot::Lazy', 'lazy gather assigned to @ keeps .elems lazy';
}

# --- other lazy-marked lists unaffected ---
is (lazy 3, 4, 5).is-lazy, True, 'lazy list literal is-lazy True';
is ((try (lazy 3, 4, 5).elems) // $!.^name), 'X::Cannot::Lazy', 'lazy list literal .elems throws';

# --- reification of a plain gather still works when assigned ---
{
    my @r = gather { take 1; take 2; take 3 };
    is @r, [1, 2, 3], 'plain gather assigned to @ reifies to the right elements';
}
