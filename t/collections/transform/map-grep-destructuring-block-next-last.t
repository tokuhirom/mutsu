use Test;

# `next`/`last` inside a map/grep block whose signature destructures its
# argument (`-> (:$key, :$value) { next if ... }`) acts on the element, as in
# any other block. Red::Column's `multi method raku` is written this way
# (Red 0.2.7 t/06-optimizer, t/24-grep-no-AND-OR, t/51-pre-create-or-update).

plan 9;

my @pairs = (a => 1), (b => 2), (c => 3);
my %h = a => 1, b => 2, c => 3;

is @pairs.map(-> (:$key, :$value) { next if $key eq 'b'; $key }).join(','), 'a,c',
    'next skips an element of an Array map';
is %h.pairs.sort.map(-> (:$key, :$value) { next if $key eq 'b'; "$key=$value" }).join(','), 'a=1,c=3',
    'next skips an element of a List map';
is @pairs.map(-> (:$key, :$value) { last if $key eq 'b'; $key }).join(','), 'a',
    'last ends an Array map';
is %h.pairs.sort.map(-> (:$key, :$value) { last if $key eq 'c'; $key }).join(','), 'a,b',
    'last ends a List map';
is @pairs.grep(-> (:$key, :$value) { next if $key eq 'b'; True }).map(*.key).join(','), 'a,c',
    'next skips an element of a grep';
my @stopped = @pairs.grep(-> (:$key, :$value) { last if $key eq 'b'; True });
is @stopped.map(*.key).join(','), 'a', 'last ends a grep';

my $seen = '';
for @pairs -> (:$key, :$value) { next if $key eq 'b'; $seen ~= $key }
is $seen, 'ac', 'a for loop over the same block shape';

class Col {
    has %.args;
    multi method raku(::?CLASS:D:) {
        "{ self.^name }.new({
            %!args.pairs.sort.map(-> (:$key, :$value) {
                next if $key eq <skip drop>.one;
                "$key.Str() => $value.raku()"
            }).join: ', '
        })"
    }
}
is Col.new(args => { a => 1, skip => 2, z => 'x' }).raku, 'Col.new(a => 1, z => "x")',
    'a method building its text with a skipping destructuring map';
is Col.new(args => { skip => 1 }).raku, 'Col.new()', 'every element skipped';
