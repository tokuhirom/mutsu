use Test;

plan 2;

is-deeply (10..12, 20).flatmap(* * 2).List,
    (20, 21, 22, 23, 24, 40),
    '.flatmap flattens a Range returned by the mapper';

is-deeply (10..^12, 20).flatmap(* * 2).List,
    (20, 21, 22, 23, 40),
    '.flatmap preserves exclusive Range boundaries while flattening';
