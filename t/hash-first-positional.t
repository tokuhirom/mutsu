use Test;

# A Hash iterates as a list of Pair values. When such a Pair is passed to a
# matcher/comparator block it must bind *positionally* (as the topic / $^a),
# not as a named argument. Regression test for `%h.first({...})` binding the
# pair as a named arg and leaving the block with zero positionals.

plan 9;

# --- %h.first with a bare block (implicit $_) ---
{
    my %h = a => 3;
    my $r = %h.first({ .value > 1 });
    isa-ok $r, Pair, '%h.first({...}) returns a Pair';
    is $r.key, 'a', '  ... correct key';
    is $r.value, 3, '  ... correct value';
}

# --- %h.first with a named sub ($p positional) ---
{
    my %h = x => 10;
    sub want-big($p) { $p.value > 5 }
    my $r = %h.first(&want-big);
    is $r.key, 'x', '%h.first(&sub) binds pair positionally (key)';
    is $r.value, 10, '%h.first(&sub) binds pair positionally (value)';
}

# --- %h.first returning Nil when nothing matches ---
{
    my %h = a => 1, b => 2;
    my $r = %h.first({ .value > 100 });
    nok $r.defined, '%h.first with no match returns undefined';
}

# --- %h.grep with a block still works (native path, regression guard) ---
{
    my %h = a => 3, b => 1, c => 2;
    my @big = %h.grep({ .value >= 2 }).sort({ $^a.key cmp $^b.key });
    is @big.elems, 2, '%h.grep({...}) keeps matching pairs';
    is @big.map(*.key).join(','), 'a,c', '  ... correct pairs by key';
}

# --- %h.sort with a 2-arg comparator block (regression guard) ---
{
    my %h = a => 3, b => 1, c => 2;
    my @sorted = %h.sort({ $^a.value <=> $^b.value }).map(*.key);
    is @sorted.join(','), 'b,c,a', '%h.sort({ $^a.value <=> $^b.value }) by value';
}
