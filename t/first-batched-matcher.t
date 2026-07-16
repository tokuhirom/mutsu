use v6;
use Test;

# Pin for the setup-once batched `.first` matcher path
# (Interpreter::try_first_match_batched, resolution_map_grep.rs). A `Sub`
# matcher is compiled once and run per element via a bare `run_reuse` with an
# early exit, instead of the ~25x-more-expensive per-element `call_sub_value`
# (call frame + scoped overlay + captured-env merge + full binder). This was
# the single hottest line of zef's Ecosystems populate
# (`.first(*.contains('<'))` over 7657 dists). The batched path must stay
# byte-identical to the generic per-element path in every observable way.

plan 15;

# Basic match / no-match
is (1, 2, 3, 4).first(* > 2), 3, 'first matches';
is (1, 2, 3, 4).first(* > 10), Nil, 'first no-match returns Nil';

# From the end
is (1, 2, 3, 4).first(* > 2, :end), 4, 'first :end scans from the end';

# Hash source: the Pair binds positionally to the block topic
{
    my %h = a => 1, b => 2, c => 3;
    is %h.first({ .value == 2 }).key, 'b', 'first over a hash binds pairs positionally';
}

# `last` control: stop the scan, return the CURRENT element as the match
is (5, 1, 2).first({ last if $_ == 1; $_ > 10 }), 1, 'last inside first returns the current element';

# `next` control: skip the current element (treat as non-matching)
is (1, 2, 3, 4).first({ next if $_ < 3; True }), 3, 'next inside first skips the element';

# Closure captures an outer lexical
{
    my $threshold = 3;
    is (1, 2, 3, 4, 5).first({ $_ > $threshold }), 4, 'first matcher captures outer lexical';
}

# Side effects run left-to-right, stopping at the first match
{
    my @seen;
    my $r = (1, 2, 3, 4).first({ @seen.push($_); $_ == 3 });
    is $r, 3, 'first stops at the match';
    is-deeply @seen, [1, 2, 3], 'first ran the matcher exactly up to the match';
}

# Inside a sub (the compiled body must treat `return` correctly — here the
# block has no return, but the enclosing sub does)
{
    sub find-big(@list) { @list.first(* > 100) // -1 }
    is find-big([5, 200, 3]), 200, 'first inside a sub';
}

# Non-Sub matchers keep the generic path (regex / Str)
is <foo bar baz>.first(/a/), 'bar', 'regex matcher still works (generic path)';
is <foo bar baz>.first('bar'), 'bar', 'Str matcher still works (generic path)';

# Empty list
is ().first(* > 2), Nil, 'first over an empty list is Nil';

# The matcher block may mutate a captured-outer lexical: its writes land in the
# shared env during the batched loop and must be reflected in the caller's slot
# on return (roast S32-list/first-kv.t "matcher got only executed once"). The
# first element (1) matches `$^x % 2`, so the block runs exactly once.
{
    my @list = 1 ... 10;
    my $count = 0;
    my $r = @list.first({ $count++; $^x % 2 });
    is $r, 1, 'first with a captured-outer-mutating matcher returns the match';
    is $count, 1, 'the outer counter mutation is written back (matcher ran once)';
}

done-testing;
