use Test;

# ADR-0058 step 3b: `.grep` produces a deferred `Seq`, exactly as `.map` does
# since steps 2/3a/3c. All four spellings ran their callback at the CALL before
# this; rakudo runs none of them until the Seq is consumed.
#
# `grep` needs something `map` has no equivalent of: on a concrete array it
# promotes every MATCHED source slot to a shared element cell and builds the
# result out of the same cells, so `for @a.grep(...) { $_++ }` mutates through
# into `@a`. Deferring moves that promotion to the pull, in a frame where the
# source's name is gone -- which only works because the promotion is published
# by mutating the source `ArrayData` in place.

plan 11;

{
    my @a = 1, 2, 3;
    my $log = '';
    my $s1 = @a.grep({ $log ~= 'A'; $_ > 1 });
    my $s2 = @a.List.grep({ $log ~= 'B'; $_ > 1 });
    my $s3 = (1, 2, 3).grep({ $log ~= 'C'; $_ > 1 });
    my $s4 = grep({ $log ~= 'D'; $_ > 1 }, @a);
    is $log, '', 'no callback has run at any of the four `.grep` calls';

    is $s1.List, (2, 3), 'an `@` receiver filters at the pull';
    is $s2.List, (2, 3), '... a `.List` receiver too';
    is $s3.List, (2, 3), '... a literal list too';
    is $s4.List, (2, 3), '... and the listop spelling too';
    is $log, 'AAABBBCCCDDD', 'every callback ran at its pull, in order';
}

is (1, 2, 3).grep({ $_ > 1 }).^name, 'Seq', '.grep answers a Seq';

# The write-back the promotion exists for, at both spellings.
{
    my @a = 1, 2, 3;
    for @a.grep({ $_ > 1 }) { $_++ }
    is @a.raku, '[1, 3, 4]', 'a writeback loop mutates through into the source';
}

{
    my @a = 1, 2, 3;
    grep({ $_ = 5 }, @a).eager;
    is @a.raku, '[5, 5, 5]', '... and the listop spelling writes back through `$_` too';
}

{
    my @a = 1, 2, 3;
    my @copy = @a.grep({ $_ > 1 });
    @copy[0] = 99;
    is @a.raku, '[1, 2, 3]', 'an `=` copy of the result decontainerizes';
}

# The adverbed forms need positional indices over the whole result, so they
# keep the eager path -- the same exemption they already take elsewhere.
{
    my @a = 1, 2, 3;
    is @a.grep({ $_ > 1 }, :k).List, (1, 2), ':k still answers the matched indices';
}
