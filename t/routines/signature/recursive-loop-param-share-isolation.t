use Test;

# Assigning a loop parameter that holds an aggregate (`$prev = $item`) shares
# the aggregate by reference. Promoting the source to a shared cell must only
# touch THIS frame's `$item`, never a caller's unrelated same-named loop
# parameter -- a recursive call used to overwrite the outer iteration's
# `$item` with the inner call's (#11304).

plan 5;

sub one(@items) {
    my $out = '';
    for @items -> $item {
        my $prev = $item;
        $out ~= one($item[1]) if $item[1];
        $out ~= "$item[0] ";
    }
    $out
}
is one([[1, [['x'], ['y']]], [2]]), 'x y 1 2 ', 'my $prev = $item in a recursive single-param loop';

sub kv(@items) {
    my $out = '';
    my $prev;
    for @items.kv -> $idx, $item {
        $out ~= kv($item<b>.list) if $item<b>;
        $out ~= "$item<a> ";
        $prev = $item;
    }
    $out
}
is kv([{a => 1, b => [{a => 'x'}, {a => 'y'}]}, {a => 2}]), 'x y 1 2 ',
    '$prev = $item in a recursive .kv loop';

class R {
    method go(@items) { self!walk(@items) }
    method !walk(@items) {
        my $out = '';
        my $prev-item = Any;
        my $renderer = self;
        for @items.kv -> $idx, $item {
            my &rec = sub (@new) { $renderer!R::walk(@new) };
            $out ~= rec($item<b>.list) if $item<b>;
            $out ~= "[$item<a> after {$prev-item ?? $prev-item<a> !! '-'}]";
            $prev-item = $item;
        }
        $out
    }
}
is R.new.go([{a => 1, b => [{a => 'x'}, {a => 'y'}]}, {a => 2}]),
    '[x after -][y after x][1 after -][2 after 1]',
    'recursion through a stored closure keeps the outer loop item';

# The share itself still works: the scalar and the loop's element alias see
# the same aggregate.
my @rows = [1], [2];
my @kept;
for @rows -> $row { my $r = $row; $r.push(9); @kept.push: $r }
is-deeply @rows, [[1, 9], [2, 9]], 'pushing through the shared scalar reaches the element';

my @outer = [1],;
sub inner { my $x = @outer[0]; $x.push(5) }
inner();
is-deeply @outer, [[1, 5],], 'sharing an element read inside a sub reaches the caller array';
