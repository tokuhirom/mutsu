use Test;

# Sub-form map()/first() over an infinite Range (1..Inf == Range(1, i64::MAX))
# must not panic with `capacity overflow` — they share grep's unified pull
# iterator now (PLAN.md §8.1 PR-2).

plan 9;

# Sub-form map on infinite range
is map(* * 2, 1..Inf)[^3].join(','), '2,4,6', 'sub map on 1..Inf, subscript ^3';
is map(* * 2, 1..Inf).head(4).join(','), '2,4,6,8', 'sub map on 1..Inf, .head(4)';

# Sub-form first on infinite range (short-circuits to the first match)
is first(* > 5, 1..Inf), 6, 'sub first(* > 5) on 1..Inf';
is first(*.is-prime, 1..Inf), 2, 'sub first(is-prime) on 1..Inf';

# Finite ranges unchanged (no regression)
is map(* * 2, 1..5).join(','), '2,4,6,8,10', 'finite sub map';
is map(* + 1, 1..^5).join(','), '2,3,4,5', 'finite sub map half-open range';
is first(* > 2, 1..10), 3, 'finite sub first';
is first(* %% 3, (2, 4, 6, 9)), 6, 'sub first over plain list';

# Method-form first on infinite range still works
is (1..Inf).first(* > 100), 101, 'method first on 1..Inf';

done-testing;
