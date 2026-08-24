use Test;

plan 5;

is (my ($a, $b)).raku, '(Any, Any)', 'grouped declaration yields every scalar';
is (my ($c, $d)).elems, 2, 'grouped declaration has the expected arity';
is (my ($e, $f)).WHAT, List, 'grouped declaration yields a List';
is (my ($g, $h) Z 1, 2).raku, '((Any, 1), (Any, 2)).Seq',
    'grouped declaration supplies every operand to zip';
ok !$a.defined && !$b.defined, 'grouped declarations remain visible in the surrounding scope';
