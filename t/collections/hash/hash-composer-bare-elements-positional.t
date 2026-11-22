use Test;

# A hash composer that holds bare elements after a pair pairs its elements in
# the order they are written, as in Rakudo. `{ a => 1, $k, 2 }` used to fail to
# parse ("Malformed initializer"), and a literal after a bare element was taken
# as the key of the next one regardless of where it sat in the list.

plan 6;

my $k = "z";
is-deeply { a => 1, $k, 2 }, { a => 1, z => 2 }, 'a variable key after a pair';
is-deeply { a => 1, "b", 2 }, { a => 1, b => 2 }, 'a quoted key after a pair';
is-deeply { a => 1, $k, "b", 2, 3 }, { a => 1, z => "b", 2 => 3 }, 'elements pair in written order';
is-deeply { a => 1, 1, 2 }, { a => 1, 1 => 2 }, 'a numeric key after a pair';
my $h = { a => 1, $k, 2 };
isa-ok $h, Hash, 'the composer is a Hash';
is-deeply { "x" => 1, "y" => 2 }, { x => 1, y => 2 }, 'quoted pairs are unchanged';
