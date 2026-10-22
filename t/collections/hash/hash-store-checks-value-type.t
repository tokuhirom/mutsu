use Test;

plan 11;

# `Hash.STORE` checks each stored value against the hash's element type before
# it replaces the pairs, as `Array.STORE` does for an array: a refused store
# leaves the hash as it was.
my Int %h = a => 1, b => 2;

throws-like { %h.STORE((a => "str", b => "2")) }, X::TypeCheck::Assignment, "a Pair list with a Str value is refused";
is-deeply %h, (my Int %= a => 1, b => 2), "and the hash is unchanged";

lives-ok { %h.STORE((a => 5, b => 6)) }, "a Pair list of Ints is stored";
is %h<a>, 5, "the new value is there";

throws-like { %h.STORE(("x", 9, "y", "bad")) }, X::TypeCheck::Assignment, "a flat key/value list is checked on its values";
lives-ok { %h.STORE(("x", 9, "y", 10)) }, "a flat key/value list of Ints is stored";
is-deeply %h.sort.map(*.value).List, (9, 10), "the flat list replaced the pairs";

throws-like { %h.STORE({k => "s"}) }, X::TypeCheck::Assignment, "a Hash argument is checked on its values";

my $obj = Hash[Int].new('a', 1);
throws-like { $obj.STORE((a => "x")) }, X::TypeCheck::Assignment, "a Hash[Int] object checks too";

my %plain = a => "x";
lives-ok { %plain.STORE((z => "any value")) }, "an untyped hash stores any value";
is %plain<z>, "any value", "and keeps it";
