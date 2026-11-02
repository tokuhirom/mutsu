use Test;

plan 12;

# An infinite Range end stringifies as `*` and the Range is never expanded
# (#16072).
is (1..Inf).Str, '1..*', 'Int start, Inf end';
is (1..*).Str, '1..*', 'Int start, Whatever end';
is (1..^Inf).Str, '1..^*', 'excluded Inf end';
is (-Inf..5).Str, '*..5', '-Inf start does not expand';
is (-Inf..^5).Str, '*..^5', '-Inf start, excluded end';
is (-Inf^..5).Str, '*^..5', '-Inf start, excluded start';
is (*..5).Str, '*..5', 'Whatever start';
is (1.5..Inf).Str, '1.5..*', 'Rat start, Inf end';
is (-Inf..Inf).Str, '*..*', 'both ends infinite';
is (1..Inf).chars, 4, '.chars of an Inf-ended Range';
is "{1..Inf}", '1..*', 'interpolated';
is (1..5).Str, '1 2 3 4 5', 'a finite Range still expands';
