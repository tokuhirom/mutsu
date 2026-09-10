use Test;

# A Hash iterates as a list of Pair values. When such a Pair is passed to a
# matcher/comparator block it must bind *positionally* (as the topic / $^a),
# not as a named argument. Regression test for `%h.first({...})` binding the
# pair as a named arg and leaving the block with zero positionals.

plan 16;

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

# --- %h.first with a pointy block (-> $p): empty param_defs legacy path ---
# A pointy lambda compiles to params=["p"], param_defs=[], so it reaches the
# legacy binding path. The Pair element (passed as ValuePair(Str,_)) must bind
# to $p positionally, not be siphoned off as a named arg.
{
    my %h = a => 1, b => 3, c => 2;
    my $r = %h.first(-> $p { $p.value > 2 });
    isa-ok $r, Pair, '%h.first(-> $p {...}) returns a Pair';
    is $r.key, 'b', '  ... pointy param binds pair positionally (key)';
    is $r.value, 3, '  ... pointy param binds pair positionally (value)';
}

# --- pointy on a plain list of Pairs (non-Hash source, same root cause) ---
{
    my @a = (a => 1), (b => 3), (c => 2);
    my $r = @a.first(-> $p { $p.value > 2 });
    is $r.key, 'b', '@pairs.first(-> $p {...}) binds positionally';
}

# --- pointy assigned to &var then passed (still a Lambda, empty param_defs) ---
{
    my %h = x => 10;
    my &big = -> $p { $p.value > 5 };
    is %h.first(&big).key, 'x', '%h.first(&pointy-var) binds positionally';
}

# --- :k adverb path (interp .first path), ordered list for deterministic index ---
{
    my @a = (a => 1), (b => 3), (c => 2);
    is @a.first(-> $p { $p.value > 2 }, :k), 1,
        '@pairs.first(-> $p {...}, :k) returns matching index';
}

# --- regression guard: Int list + pointy still works (no Pair involved) ---
{
    my @n = 1, 5, 2, 8;
    is @n.first(-> $x { $x > 3 }), 5, '@ints.first(-> $x {...}) unaffected';
}
