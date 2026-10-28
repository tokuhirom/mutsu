use Test;

plan 14;

# A class that `does Sequence` with its own `iterator` answers Any's
# `self.list` methods (`pairs`, `kv`, `keys`, ...) from its elements,
# including when a role is mixed into an instance (Red::ResultSeq).
class It does Iterator {
    has $.n = 0;
    method pull-one { $!n < 6 ?? ($!n++ % 3) !! IterationEnd }
}
class S does Sequence { method iterator { It.new } }
role R { method foo { 1 } }

for 'plain' => S.new, 'mixin' => (S.new but R) -> $case {
    my $s = $case.value;
    is-deeply $s.pairs.list, (0 => 0, 1 => 1, 2 => 2, 3 => 0, 4 => 1, 5 => 2).list,
        "$case.key(): pairs";
    is-deeply $s.kv.list, (0, 0, 1, 1, 2, 2, 3, 0, 4, 1, 5, 2).list, "$case.key(): kv";
    is-deeply $s.keys.list, (0, 1, 2, 3, 4, 5).list, "$case.key(): keys";
    is-deeply $s.unique.list, (0, 1, 2).list, "$case.key(): unique";
    is-deeply $s.squish.list, (0, 1, 2, 0, 1, 2).list, "$case.key(): squish";
}
is (S.new but R).foo, 1, 'the mixed-in role method is still callable';
is (S.new but R).elems, 6, 'elems';
is-deeply (S.new but R).rotor(2).list, ((0, 1), (2, 0), (1, 2)).list, 'rotor';
is-deeply S.new.rotor(2).list, ((0, 1), (2, 0), (1, 2)).list, 'rotor on the plain class';
