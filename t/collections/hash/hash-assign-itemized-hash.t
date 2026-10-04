use Test;

# Found via the ML::SparseMatrixRecommender suite (Math::SparseMatrix's
# `set-column-names`: `%!column-names-map = @res.tail`, then
# `%!column-names-map , $other.column-names-map`).
plan 7;

# Assigning an itemized Hash to a `%` variable stores its entries, not the item.
sub assign-param($x) { my %h; %h = $x; %h<z> = 9; %h }
my $item = {a => 1};
is-deeply assign-param($item).keys.sort.List, <a z>, 'entries of an itemized hash param';
is-deeply $item.keys.List, ('a',), 'the source hash is not aliased';

class Holder {
    has %.m;
    method set($x) { %!m = $x; self }
    method merged-with($other) { my %n = %!m, $other.m; %n.elems }
}
my @pair = [1, {a => 0, b => 1}];
is Holder.new.set(@pair.tail).merged-with(Holder.new(m => {c => 2})), 3, 'attribute assigned from an Array element hash';
is Holder.new.set($item).merged-with(Holder.new(m => {c => 2})), 2, 'attribute assigned from an itemized scalar';
is Holder.new.set($item).m.raku, '{:a(1)}', '.raku shows no item marker';

my %top; %top = $item; %top<q> = 1;
is-deeply $item.keys.List, ('a',), 'top-level assignment does not alias either';
my %merged = %top, {c => 2}; is %merged.elems, 3, 'list assignment sees the entries';

