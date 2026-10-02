use Test;

plan 36;

# `Hash`/`Map` (and `Set`/`Bag`/`Mix`) inherit `reverse`, `unique`, `squish`,
# `eager`, `Seq`, `Supply`, `minmax` and `produce` from `Any`, which defines
# each as `self.list.METHOD`: on such a receiver the invocant IS its list of
# Pairs. mutsu treated the hash as ONE opaque item (or, for `reverse`, had no
# such method at all). Hash order is arbitrary, so multi-key results are sorted.

my %s = a => 1;
my %t = a => 1, b => 2;
my %e;

# --- a one-key hash: every result is fully determined -------------------------
is %s.reverse.List.raku,    '(:a(1),)',     '.reverse';
is %s.unique.List.raku,     '(:a(1),)',     '.unique';
is %s.squish.List.raku,     '(:a(1),)',     '.squish';
is %s.Seq.List.raku,        '(:a(1),)',     '.Seq';
is %s.Seq.^name,            'Seq',          '.Seq is a Seq';
is %s.eager.List.raku,      '(:a(1),)',     '.eager';
is %s.Supply.list.List.raku, '(:a(1),)',    '.Supply';
is %s.minmax.raku,          ':a(1)..:a(1)', '.minmax of one pair';

# --- two keys ------------------------------------------------------------------
is %t.reverse.sort.List.raku, '(:a(1), :b(2))', '.reverse keeps both pairs';
is %t.unique.sort.List.raku,  '(:a(1), :b(2))', '.unique keeps both pairs';
is %t.squish.sort.List.raku,  '(:a(1), :b(2))', '.squish keeps both pairs';
is %t.Seq.sort.List.raku,     '(:a(1), :b(2))', '.Seq keeps both pairs';
is %t.Seq.elems,              2,                '.Seq has one element per pair';
is %t.minmax.raku,            ':a(1)..:b(2)',   '.minmax orders the pairs';
is %t.produce({ $^a.key ~ $^b.key }).elems, 2,  '.produce runs over the pairs';

# --- the empty hash ------------------------------------------------------------
is %e.reverse.List.raku, '()', 'empty .reverse';
is %e.unique.List.raku,  '()', 'empty .unique';
is %e.squish.List.raku,  '()', 'empty .squish';
is %e.Seq.List.raku,     '()', 'empty .Seq';
is %e.eager.List.raku,   '()', 'empty .eager';

# --- every kind of hash receiver -----------------------------------------------
{
    my $held = {a => 1};
    is $held.reverse.List.raku, '(:a(1),)', 'a `$`-held (itemized) hash: .reverse';
    is $held.Seq.List.raku,     '(:a(1),)', 'a `$`-held (itemized) hash: .Seq';
    is Map.new((a => 1)).reverse.List.raku, '(:a(1),)', 'a Map: .reverse';
    is Map.new((a => 1)).unique.List.raku,  '(:a(1),)', 'a Map: .unique';
    my %o{Any} = 1 => 2;
    is %o.reverse.List.raku, '(1 => 2,)', 'an object hash keeps its key OBJECTS';
}

# --- Set / Bag / Mix: the same Any methods -------------------------------------
{
    is bag(<a a b>).reverse.sort.List.raku, '(:a(2), :b(1))', 'Bag: .reverse';
    is bag(<a a b>).unique.sort.List.raku,  '(:a(2), :b(1))', 'Bag: .unique';
    is bag(<a a b>).Seq.sort.List.raku,     '(:a(2), :b(1))', 'Bag: .Seq';
    is set(<a b>).reverse.sort.List.raku,   '(:a, :b)',       'Set: .reverse';
    is set(<a b>).unique.sort.List.raku,    '(:a, :b)',       'Set: .unique';
    is mix(<a b>).squish.sort.List.raku,    '(:a(1), :b(1))', 'Mix: .squish';
    is mix(<a b>).minmax.raku,              ':a(1)..:b(1)',   'Mix: .minmax';
}

# --- the Hash-valued results of duckmap/deepmap/nodemap ------------------------
# They map over the VALUES and rebuild a real Hash, so a Boolean result is stored
# the way a Hash element store does (the long `:a(Bool::True)` form).
{
    is %s.duckmap({ .defined }).raku, '{:a(Bool::True)}', 'duckmap: a Bool value';
    is %s.deepmap({ .defined }).raku, '{:a(Bool::True)}', 'deepmap: a Bool value';
    is %s.nodemap({ .defined }).raku, '{:a(Bool::True)}', 'nodemap: a Bool value';
    is %s.duckmap({ $_ + 1 }).raku,   '{:a(2)}',          'duckmap: a non-Bool value is unchanged';
}
