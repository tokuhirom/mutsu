use Test;

# `.max`/`.min` with :k/:v/:kv/:p answer a List (not a Seq), and on a Hash they
# answer the extremal Pairs' keys/values/pairs, not indices (ParaSeq's
# t/30-max.rakutest .. t/33-minpairs.rakutest).
plan 20;

my @l = 3, 9, 2, 9, 5;

isa-ok @l.max(:p), List, "Array.max(:p) is a List";
isa-ok @l.min(:p), List, "Array.min(:p) is a List";
isa-ok @l.max(:kv), List, "Array.max(:kv) is a List";
isa-ok @l.min(:kv), List, "Array.min(:kv) is a List";
isa-ok @l.max(:k), List, "Array.max(:k) is a List";
isa-ok @l.min(:k), List, "Array.min(:k) is a List";
isa-ok @l.max(:v), List, "Array.max(:v) is a List";
isa-ok @l.min(:v), List, "Array.min(:v) is a List";

is-deeply @l.max(:p), (1 => 9, 3 => 9), "max :p";
is-deeply @l.max(:kv), (1, 9, 3, 9), "max :kv";
is-deeply @l.max(:k), (1, 3), "max :k";
is-deeply @l.max(:v), (9, 9), "max :v";
is-deeply @l.min(:p), (2 => 2,), "min :p";

my %h = a => 1, b => 5, c => 5;
is-deeply %h.max(:k).sort.List, ("b", "c"), "Hash max :k is the keys";
is-deeply %h.max(:v), (5, 5), "Hash max :v";
is-deeply %h.max(:p).sort.List, (:b(5), :c(5)), "Hash max :p";
is-deeply %h.max(:kv).pairup.sort.List, (:b(5), :c(5)), "Hash max :kv";
is-deeply %h.min(:p), (:a(1),), "Hash min :p";
isa-ok %h.max(:p), List, "Hash max(:p) is a List";
is-deeply [].max(:p), (), "empty list";
