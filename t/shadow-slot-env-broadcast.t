use Test;

plan 8;

# §1.4 shadow slots (docs/lexical-scope-slot-campaign.md, S12): a shadowing
# inner-block `my @a`/`my %h` occupies its own locals slot under
# MUTSU_SHADOW_SLOTS, so the whole-locals env broadcast that `say` (and the
# regex-interpolation sync) runs must NOT push an uninitialized same-named
# sibling slot over the live value (env is name-keyed and holds one value per
# name). Must pass identically with the gate OFF (no duplicate slots exist)
# and ON (`dup_named_locals` skips the ambiguous names).

my @a = 1, 2;
{
    my @a = "foo";
    my @b = @a.reverse;
    my $b = @a.reverse;
    # `say` triggers the whole-locals env broadcast; with a later sibling
    # `my @a` below, an unskipped broadcast clobbered env<@a> with Nil.
    say "# probe=", @b[0];
    is @a[0], "foo", 'shadowed @a survives the say env broadcast';
    is ~$b, "foo", 'Seq over the shadowed @a reads the live value';
    say "# probe2=", $b;
    is @a[0], "foo", 'shadowed @a survives a second broadcast';
}
{
    my @a = "x", "y";
    say "# probe3=", @a[0];
    is @a[0], "x", 'sibling shadow block sees its own @a';
}
is @a[0], 1, 'outer @a restored after the shadow blocks';

# The regex-interpolation env sync has the same broadcast shape: a shadowed
# %h must stay readable with $/ / $0 hash keys right after a match
# (roast/S02-types/hash.t #80-81).
my %h;
{
    my %h = (ab => 'x', 'a' => 'y');
    'abc' ~~ /^(.)./;
    is %h{$/}, 'x', 'shadowed %h readable with $/ as hash key';
    is %h{$0}, 'y', 'shadowed %h readable with $0 as hash key';
}

# For-topic container writeback targets the shadowed source's own slot via
# the slot baked on TagContainerRef (roast/S32-list/reverse.t #21).
{
    my @a = 1..3;
    {
        my Int @a = ^5;
        @a[2]:delete;
        my int $i = 0;
        $_ = ++$i for @a.reverse;
        is-deeply @a, Array[Int].new(5, 4, 3, 2, 1),
            'for-topic writeback lands on the shadowed @a';
    }
}
