use Test;

# `.grep` over an Array promotes each matched source slot to a shared element
# cell, so a writeback loop mutates through into the source. That promotion is
# published on the source `ArrayData` itself, which means a later reader of the
# same array sees element containers where it used to see bare values. Every
# reader has to look through them.
#
# `Backtrace` is the case that found this: its frame readers matched
# `ValueView::Instance` directly, so after `$bt.grep(...)` the whole backtrace
# read as empty -- `.summary` came back '' and `.nice` lost its filtering.

plan 6;

{
    my sub bar { die }();
    CATCH {
        default {
            my $bt = .backtrace;
            my $before = $bt.summary;
            ok $before.chars > 0, '.summary is non-empty to begin with';
            $bt.grep({ !.is-hidden && .is-routine });
            is $bt.summary, $before, '.summary survives a `.grep` over the same backtrace';
            is $bt.full.chars > 0, True, '... and so does .full';
            is $bt.list.elems > 0, True, '... and so does .list';
        }
    }
}

# The writeback the promotion exists for must keep working.
{
    my @a = 1, 2, 3;
    for @a.grep({ $_ > 1 }) { $_++ }
    is @a.raku, '[1, 3, 4]', 'a writeback loop still mutates through the grep result';
}

{
    my @b = 1, 2, 3;
    my @c = @b.grep({ $_ > 1 });
    @c[0] = 99;
    is @b.raku, '[1, 2, 3]', '... while an `=` copy of the result decontainerizes';
}
