use v6;
use Test;

plan 8;

# A `does Sequence` instance keeps its iterator-driven list protocol when a
# role is mixed into it with `but`.
class C does Sequence { method iterator { (4, 5).iterator } }
role Extra { has $.x = 1 }
my $m = C.new but Extra;

is $m.Str, "4 5", ".Str goes through the iterator";
is ~$m, "4 5", "prefix ~";
is "[$m]", "[4 5]", "interpolation";
is "<" ~ $m ~ ">", "<4 5>", "infix ~";
ok $m eq "4 5", "infix eq";
is $m.list.elems, 2, ".list drains the iterator";
is $m.join(","), "4,5", ".join";
is $m.x, 1, "the mixed-in role is still there";
