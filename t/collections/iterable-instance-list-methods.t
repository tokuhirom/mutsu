use v6;
use Test;

# A user class that does Iterable and defines its own `iterator` method is
# list-like for Any's ITERATION methods: .first/.map/.grep/.sort/.head/.tail/
# .flat operate on the iterator's elements, not on the instance as a single
# opaque item. (Text::CSV's CSV::Row predefined filters rely on this: the
# not_empty hook is `{ $^row.first: { .defined && $_ ne "" } }` — treating the
# row as one item made every multi-field all-empty row pass the filter.)
# NOTE (measured against rakudo 2026-08-12): join/reverse/list/List/elems/kv/
# pairs/values/Array/cache/eager do NOT iterate such an instance — they treat
# it as one item — so they are deliberately absent here.

plan 11;

class Row does Iterable does Positional {
    has @.fields;
    method iterator () { @!fields.iterator }
    method AT-POS (int $i) { @!fields.AT-POS($i) }
    method elems { @!fields.elems }
}

my $empty = Row.new(fields => ["", ""]);
my $mixed = Row.new(fields => ["", "x", ""]);

# .first iterates the fields, so an all-empty row yields no match
my $hit = $empty.first: { .defined && $_ ne "" };
nok $hit.defined, '.first on all-empty Iterable instance finds nothing';
is ($mixed.first: { .defined && $_ ne "" }), "x",
    '.first on Iterable instance finds the matching element';

# .map / .grep iterate elements
is $mixed.map({ "<$_>" }).join("|"), "<>|<x>|<>",
    '.map iterates the user iterator elements';
is $mixed.grep({ $_ ne "" }).elems, 1, '.grep iterates elements';

# .sort / .head / .tail / .flat
is Row.new(fields => ["b", "a", "c"]).sort.join(""), "abc", '.sort iterates elements';
is $mixed.head, "", '.head reads the first element';
is $mixed.tail, "", '.tail reads the last element';
is $mixed.flat.elems, 3, '.flat iterates elements';

# A method the class itself defines still wins over the routed fallback
is $mixed.elems, 3, "user-defined elems method still dispatches";

# join does NOT iterate: the instance is one item (rakudo-verified)
my $joined = Row.new(fields => ["1", "2"]).join(",");
nok $joined.contains(","), '.join treats the instance as a single item';

# elems without a user method: one item, not the iterator length
class Row2 does Iterable {
    method iterator () { ("a", "b", "c").iterator }
}
is Row2.new.elems, 1, '.elems treats the instance as a single item';
