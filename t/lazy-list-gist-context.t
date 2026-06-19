use Test;

# A genuinely-lazy `Value::LazyList` (gather coroutine, sequence, pipeline)
# renders a bounded placeholder instead of forcing the (possibly infinite)
# sequence. The placeholder is context-sensitive: `[...]` when held in an `@`
# array, `(...)` for a bare Seq scalar, `...` under Str/interpolation.

plan 17;

# --- infinite gather held in an @ array ---------------------------------
{
    my @a = lazy gather { for 1..* { take $_ } };
    is @a.WHAT.gist, '(Array)', 'lazy gather in @a reports Array';
    is @a.gist, '[...]', 'lazy gather array gist is [...]';
    is @a.Str, '...', 'lazy gather array Str is ...';
    is @a.raku, '[...]', 'lazy gather array raku is [...]';
    is @a[5], 6, 'lazy gather array still reifies on index';
    is @a.head(3).gist, '(1 2 3)', 'lazy gather array head still works';
}

# --- infinite gather held in a $ scalar (Seq) ---------------------------
{
    my $s = lazy gather { for 1..* { take $_ } };
    is $s.WHAT.gist, '(Seq)', 'lazy gather in $s reports Seq';
    is $s.gist, '(...)', 'lazy gather scalar gist is (...)';
    is $s.Str, '...', 'lazy gather scalar Str is ...';
    is "val=$s", 'val=...', 'lazy gather scalar interpolates as ...';
}

# --- a FINITE lazy gather is still is-lazy, so it also placeholders ------
{
    my @a = lazy gather { take 1; take 2; take 3 };
    is @a.gist, '[...]', 'finite lazy gather array gist is [...]';
    my $s = lazy gather { take 1; take 2; take 3 };
    is $s.gist, '(...)', 'finite lazy gather scalar gist is (...)';
}

# --- a PLAIN (non-lazy) gather still materializes -----------------------
{
    my @a = gather { take 1; take 2 };
    is @a.is-lazy, False, 'plain gather is not lazy';
    is @a.gist, '[1 2]', 'plain gather array gist materializes';
}

# --- infinite range / sequence / map placeholders unchanged -------------
{
    my @a = 1..*;
    is @a.gist, '[...]', 'infinite range array gist is [...]';
    my $seq = (1 ... *);
    is $seq.gist, '(...)', 'infinite sequence scalar gist is (...)';
    my $m = (1..*).map(* + 1);
    is $m.gist, '(...)', 'infinite map pipeline scalar gist is (...)';
}
