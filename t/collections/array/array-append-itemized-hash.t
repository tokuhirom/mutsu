use v6;
use Test;

# `.append`/`.prepend` follow the one-arg rule: a single *unitemized* Hash
# argument flattens into its key-value Pairs, but a `$`-itemized one (a
# Hash held in a Scalar container, e.g. `my $h = {a=>1}`) is a single
# element and must NOT flatten -- the same distinction `flatten_append_args`
# (src/runtime/mod.rs) already made for an Array argument via
# `!kind.is_itemized()`, just missing on the Hash arm.
#
# Found via Text::SubParsers 0.1.4's `get-matches`, which builds each
# match's parsed value in `my $res = ...;` (itemizing it when it's a Hash
# from `from-json`) and appends it to an accumulator list expecting one
# Hash per successful match, not its pairs spread across the list.

plan 6;

my %h = a => 1, b => 2;
my @flat;
@flat.append(%h);
is-deeply @flat.sort(*.key), (a => 1, b => 2).sort(*.key),
    "an unitemized Hash argument still flattens into pairs (unaffected)";

my $ih = { a => 1, b => 2 };
my @kept;
@kept.append($ih);
is @kept.elems, 1, "a \$-itemized Hash argument does not flatten (one element)";
isa-ok @kept[0], Hash, "...and that element is the Hash itself";
is-deeply @kept[0], { a => 1, b => 2 }, "...with the right contents";

my @prep;
@prep.prepend($ih);
is @prep.elems, 1, "prepend follows the same itemized-Hash rule";
isa-ok @prep[0], Hash, "...and keeps it as one Hash element";
