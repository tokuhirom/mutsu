use Test;

# A multi-parameter `for` loop binds each batch to its parameters; nested
# loops, closures over the parameters and a short last batch each see their
# own batch.

plan 7;

my @out;
for 1, 2, 3, 4 -> $a, $b {
    for <x y z w> -> $c, $d {
        @out.push: "$a$b$c$d";
    }
    @out.push: "$a$b";
}
is @out.join(' '), '12xy 12zw 12 34xy 34zw 34', 'nested multi-parameter loops';

my @subs;
for 1, 2, 3, 4 -> $k, $v { @subs.push: -> { "$k=$v" } }
is @subs.map({ .() }).join(','), '1=2,3=4', 'closures capture each batch';

my %h = a => 1, b => 2, c => 3;
my @kv;
for %h.sort(*.key).map({ |(.key, .value) }) -> $k, $v { @kv.push: "$k$v" }
is @kv.join(','), 'a1,b2,c3', 'pairs flattened into a two-parameter loop';

my @short;
for 1, 2, 3 -> $a, $b? { @short.push: $b // 'none' }
is @short.join(','), '2,none', 'an optional parameter of a short last batch';

throws-like { for 1, 2, 3 -> $a, $b { } }, X::AdHoc,
    message => /'Too few positionals passed'/, 'a short last batch dies';

class C {
    has %.h = x => 1, y => 2;
    method pairs-str { my @r; for %!h.sort(*.key).map({ |(.key, .value) }) -> $k, $v { @r.push: "$k:$v" }; @r.join(' ') }
}
is C.new.pairs-str, 'x:1 y:2', 'a multi-parameter loop in a method';

my $total = 0;
for ^3 {
    for 1, 2, 3, 4 -> $a, $b { $total += $a * $b }
}
is $total, 42, 'a multi-parameter loop re-entered from an outer loop';
