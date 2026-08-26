use Test;

# Lazy evaluation, gather/take, and Junction repr/construction.
# Every assertion below was established by running the same code under
# `raku` first; where rakudo does not promise an order (hyper dispatch,
# Junction eigenstate rendering) the assertion is deliberately order-free.

plan 34;

# ---------------------------------------------------------------------------
# `lazy { BLOCK }` statement prefix
# ---------------------------------------------------------------------------

# The block is evaluated EAGERLY; only the resulting list is marked lazy.
my @log;
my $eagerly = lazy { @log.push("ran"); 1, 2, 3 };
is @log.join(","), "ran", 'lazy BLOCK runs its block immediately';
is $eagerly.WHAT.gist, '(Seq)', 'lazy BLOCK produces a Seq';
ok $eagerly.is-lazy, 'the Seq lazy BLOCK produces is .is-lazy';

my @array = lazy { (^3).map( * ** 2 ) };
ok @array.is-lazy, 'a lazy BLOCK assigned to an @array keeps it lazy';
is @array.gist, '[...]', 'an unforced lazy array gists as the [...] placeholder';
is @array.eager.gist, '[0 1 4]', '.eager forces the lazy array to its elements';
is @array[1], 1, 'indexing a lazy array reifies just enough of it';

# ---------------------------------------------------------------------------
# `gather` sequences feeding hyper / .map / .grep
# ---------------------------------------------------------------------------

# A plain `gather` is NOT lazy in Rakudo, and neither is a .map/.grep pipe
# built on top of one.
ok !(gather { take 1; take 2 }).is-lazy, 'a plain gather Seq is not .is-lazy';
ok !(gather { take 1; take 2 }).map(* + 1).is-lazy,
        'a .map pipe over a gather is not .is-lazy either';
ok (1..Inf).map(* + 1).is-lazy, 'a .map pipe over an infinite source stays lazy';

# Hyper dispatch does not promise an order, so compare order-insensitively.
my $hyper-src = gather { take 1; take 2; take 3 };
is ($hyper-src>>.Str).sort.join(","), "1,2,3",
        'a hyper method call forces the gather Seq instead of seeing it empty';

# .map/.grep are ordered.
my $mapped = gather { take 1; take 2 };
is $mapped.map(* + 1).join(","), "2,3", '.map over a gather Seq maps its elements';

my $grepped = gather { take 1; take 2; take 3 };
is $grepped.grep(* > 1).join(","), "2,3", '.grep over a gather Seq filters its elements';

my $counted = gather { take 1; take 2 };
my $pipe = $counted.map(* + 1);
is $pipe.elems, 2, '.elems on a finite pipe over a gather counts instead of throwing';

# ---------------------------------------------------------------------------
# A `FIRST` phaser that exits its iteration via `next`
# ---------------------------------------------------------------------------

my @seen;
for 1..3 { FIRST next; @seen.push($_) }
is @seen.join(","), "2,3", 'FIRST runs once even when it exits via next';

my @first-runs;
for 1..3 { FIRST @first-runs.push($_), next; }
is @first-runs.join(","), "1", 'a FIRST body that throws next still runs exactly once';

is (gather for <a b c> { FIRST .take, next; take slip ":", .item }).join(" "),
        "a : b : c",
        'a take of a Slip after a next-exiting FIRST keeps both elements';

sub insert($sep, +@list) {
    gather for @list {
        FIRST .take, next;
        take slip $sep, .item
    }
}
is insert(':', <a b c>).join(" "), "a : b : c", 'the documented insert() idiom';

# ---------------------------------------------------------------------------
# `take-rw` keeps a live container alias
# ---------------------------------------------------------------------------

my @src = 1, 2, 3;
my @alias := @src;             # a second name for the SAME container
for (gather { take-rw @src[0]; take-rw @src[1] }) { $_ = 7 }
# Identity, not value: a by-value snapshot could not have reached @src at all,
# and could not have been visible through the independently-bound @alias.
is @src.join(","), "7,7,3", 'writing through a take-rw alias mutates the source';
is @alias.join(","), "7,7,3", 'the mutation is visible through every alias of the container';

my @untouched = 1, 2, 3;
my @copied = gather { take @untouched[0] };
$_ = 42 for @copied;
is @untouched.join(","), "1,2,3", 'a plain take copies, so the source is untouched';

# ---------------------------------------------------------------------------
# `Junction.new` flattens any iterable values argument
# ---------------------------------------------------------------------------

nok Junction.new("one", 1..6).Bool,
        'Junction.new flattens a Range into eigenstates (one() of 6 truthy is False)';
ok Junction.new("one", 1..1).Bool, 'a one-element Range junction is still one()';
is Junction.new("any", 1..6).raku.comb(/\d/).sort.join(","), "1,2,3,4,5,6",
        'all six Range eigenstates are present';
is Junction.new("any", (1, 2)).raku.comb(/\d/).sort.join(","), "1,2",
        'a plain list values argument still flattens';
is Junction.new("any", "abc").raku, 'any("abc")', 'a Str values argument stays one eigenstate';
is Junction.new("any", 5).raku, 'any(5)', 'an Int values argument stays one eigenstate';
is any(1..3).raku.comb(/\d/).sort.join(","), "1,2,3", 'any(Range) has the same eigenstates';

# ---------------------------------------------------------------------------
# `.raku` on a Junction held in a variable
# ---------------------------------------------------------------------------

my $j = any("5", "6");
# Order-free: count the quote characters and look for each quoted member.
is $j.raku.comb(/\"/).elems, 4, '.raku through a variable quotes EVERY Str eigenstate';
ok $j.raku.contains('"5"'), '.raku quotes the first eigenstate';
ok $j.raku.contains('"6"'), '.raku quotes the last eigenstate';
is $j.perl, $j.raku, '.perl agrees with .raku';
is $j.gist.comb(/\"/).elems, 0, '.gist still renders eigenstates unquoted';
is any("5", "6").raku, $j.raku, 'the no-variable spelling renders identically';
