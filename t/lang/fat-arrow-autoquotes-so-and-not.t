use v6;
use Test;

# `=>` autoquotes the identifier before it, including the loose prefix
# operators `so` and `not` (Color::DirColors' GNU type map has `so => 'socket'`).

plan 5;

is-deeply (so => 1), Pair.new('so', 1), '`so => 1` is a Pair';
is-deeply (not => 1), Pair.new('not', 1), '`not => 1` is a Pair';
my %h = pi => 'pipe', so => 'socket', do => 'door';
is %h<so>, 'socket', 'a `so` key in a hash list';
is (so 1), True, 'prefix `so` still works';
is (not 0), True, 'prefix `not` still works';
