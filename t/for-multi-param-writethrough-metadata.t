use Test;

# Three general bugs found while fixing
# todo/tickets/for-multi-param-array-hash-shadow-clobbers-outer-container.md
# (that fix routes an `@`/`%`-sigil multi-param loop variable through the same
# bind (`:=`) machinery `my @a := expr` already used, which surfaced these
# pre-existing, independent gaps in that machinery — none specific to `for`).

plan 8;

# 1. A `:=`-bound native-typed array reassigned from a closure that only
# writes it "by name" (no local slot of its own for the free var) used to
# lose its element type, collapsing `array[int]` to a plain `Array`.
{
    my int @src = 1, 2, 3;
    my @a := @src;
    sub inner(\values) { @a = values }
    inner((4, 5, 6));
    is @a.WHAT.^name, 'array[int]', 'closure write to a bound native array keeps its element type';
    is-deeply @a.List, (4, 5, 6), 'and the values themselves are correct';
}

# 1b. The same array must still correctly reject/throw for an infinite
# source (X::Cannot::Lazy) instead of eagerly materializing it — a
# regression caught while fixing the above (a redundant re-coercion of an
# already-lazy-tagged array silently dropped its lazy marker).
{
    my num @arr;
    my $blk = { @arr = 0e0..Inf };
    throws-like { $blk.() }, X::Cannot::Lazy, 'closure write of an infinite Range to a native array still throws';
}

# 2. `%x >>[&op]=<< %y` (hash hyper-meta-assignment via a bracketed function)
# computed the right-hand values but never wrote them back to `%x` — the
# compiler's hyper-assign lvalue dispatch had no case for a hash variable.
{
    sub op($x, $y) { ($x + $y).Str }
    my %x = a => 1, b => 2;
    my %y = a => 3, b => 4;
    %x >>[&op]=<< %y;
    is-deeply %x, { a => "4", b => "6" }, 'hash >>[&op]=<< writes its result back to the hash variable';
}

# 2b. The symbolic form (`>>op=<<`) already worked for the value computation
# but is included here as a same-family regression guard.
{
    my %x = a => 1, b => 2;
    my %y = a => 3, b => 4;
    %x >>+=<< %y;
    is-deeply %x, { a => 4, b => 6 }, 'hash >>+=<< writes its result back to the hash variable';
}

# 3. A hyper op combining two OBJECT hashes (`%h{Any}`, `.WHICH`-keyed) lost
# the result's object-hash identity — both the symbolic and the bracketed
# function-op forms rebuilt a plain (string-keyed) Hash, discarding
# `key_type`/`original_keys`, so `.raku` rendered raw `.WHICH` strings as
# literal keys instead of the real key objects.
{
    my %a{Any} = "a" => 1, "b" => 2;
    my %b{Any} = "a" => 3, "b" => 4;
    my $r = %a >>+<< %b;
    is $r.raku, '$(my Any %{Any} = :a(4), :b(6))', 'object-hash hyper op (symbolic) keeps object-hash identity';
}
{
    sub op($x, $y) { ($x + $y).Str }
    my %a{Any} = "a" => 1, "b" => 2;
    my %b{Any} = "a" => 3, "b" => 4;
    my $r = %a >>[&op]<< %b;
    is $r.raku, '$(my Any %{Any} = :a("4"), :b("6"))', 'object-hash hyper op (bracketed function) keeps object-hash identity';
}
{
    sub op($x, $y) { ($x + $y).Str }
    my %a{Any} = "a" => 1, "b" => 2;
    my %b{Any} = "a" => 3, "b" => 4;
    %a >>[&op]=<< %b;
    is %a.raku, '(my Any %{Any} = :a("4"), :b("6"))', 'object-hash hyper-meta-assign keeps object-hash identity';
}
