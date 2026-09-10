use v6;
use Test;

# A container held in a `$` renders itemized -- `.raku` prefixes it with `$`.
# Mixing a role in used to drop the prefix: the itemization the `$` confers is
# applied to the value being stored, and the `Mixin` wrapper hid the container
# from that step. Only the rendering was ever wrong; the value itself always
# behaved correctly.
#
# A Set/Bag/Mix is the other way round -- raku does not itemize it, but it does
# name its own type, so a mixed one has to name the role with it.

plan 27;

role R { }

# --- The `$` itemization reaches through the mixin -------------------------
{
    my $h = { a => 1, b => 2 };
    my $mx = { a => 1, b => 2 } but R;
    is $h.raku, '${:a(1), :b(2)}', 'a plain hash in a $ is itemized';
    is $mx.raku, '${:a(1), :b(2)}', 'and so is a mixed one';
}
{
    my $a = [1, 2];
    my $ax = [1, 2] but R;
    is $a.raku, '$[1, 2]', 'a plain array in a $ is itemized';
    is $ax.raku, '$[1, 2]', 'and so is a mixed one';
}
{
    my $l = (1, 2);
    my $lx = (1, 2) but R;
    is $l.raku, '$(1, 2)', 'a plain list in a $ is itemized';
    is $lx.raku, '$(1, 2)', 'and so is a mixed one';
}

# The mixin is still a mixin, and the container still behaves like one.
{
    my $mx = { a => 1, b => 2 } but R;
    ok $mx ~~ R, 'the role is still mixed in';
    is $mx.^name, 'Hash+{R}', 'and reported by .^name';
    is-deeply ($mx<a>:delete), 1, 'a subscript still reaches the container';
    is $mx.raku, '${:b(2)}', 'and the itemization survives the mutation';
}

# A value NOT held in a `$` is not itemized, mixed or not.
{
    is ({ a => 1 } but R).raku, '{:a(1)}', 'a bare mixed hash is not itemized';
    my %plain = a => 1;
    is %plain.raku, '{:a(1)}', 'nor is a %-held one';
}

# --- A quanthash names the role in its own type ---------------------------
{
    my $s = set(<a>) but R;
    is $s.raku, 'Set+{R}.new("a")', 'a mixed Set names the role';
    is $s.gist, 'Set+{R}(a)', 'in its gist too';
    is $s.^name, 'Set+{R}', 'agreeing with .^name';
}
{
    is (SetHash.new("a") but R).raku, 'SetHash+{R}.new("a")', 'a mixed SetHash';
    is ((a => 2).Bag but R).raku, '("a"=>2).Bag+{R}', 'a mixed Bag';
    is ((a => 1.5).Mix but R).raku, '("a"=>1.5).Mix+{R}', 'a mixed Mix';
    is ((a => 2).BagHash but R).raku, '("a"=>2).BagHash+{R}', 'a mixed BagHash';
}

# An empty *immutable* quanthash renders via its lowercase coercer -- but only
# unmixed: there is no coercer spelling that carries the role.
{
    is set().raku, 'set()', 'an empty Set is its coercer';
    is (set() but R).raku, 'Set+{R}.new()', 'a mixed empty Set is not';
    is bag().raku, 'bag()', 'an empty Bag is its coercer';
    is (bag() but R).raku, '().Bag+{R}', 'a mixed empty Bag is not';
    is (mix() but R).raku, '().Mix+{R}', 'nor a mixed empty Mix';
}

# The unmixed forms are untouched.
{
    is set(<a>).raku, 'Set.new("a")', 'a plain Set';
    is (a => 2).Bag.raku, '("a"=>2).Bag', 'a plain Bag';
    is set(<a>).gist, 'Set(a)', 'a plain Set gist';
}
