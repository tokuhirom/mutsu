use Test;

plan 4;

my ($n, $from, $to) = (200, 0.1, 0.100001);
my @res = ($from .. $to).rand xx $n;
is @res.elems, $n, 'xx produces the requested element count';
is @res.grep($from <= * <= $to).elems, $n, 'chained placeholder comparisons work in grep';
ok @res.Set.elems > 1, 'xx re-evaluates lhs expression';

my %h := :{ :42foo, (True) => False, 42e0 => 1/2 };
is-deeply gather { %h.pick.take xx 200 }.unique.sort,
    (:42foo, (True) => False, 42e0 => 1/2).sort,
    'pick from typed object hashes preserves key/value pair semantics';
