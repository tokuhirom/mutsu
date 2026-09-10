use Test;

# grep over an infinite Range (1..Inf == Range(1, i64::MAX)) must stay lazy
# enough to be subscripted/headed without panicking with `capacity overflow`
# (ANALYSIS §8.2 / PLAN.md §8.1 pull-model unification).

plan 10;

# Method form
is (1..Inf).grep(* %% 2)[^3].join(','), '2,4,6', 'method grep on 1..Inf, subscript ^3';
is (1..Inf).grep(* %% 2).head(3).join(','), '2,4,6', 'method grep on 1..Inf, .head(3)';
is (1..Inf).grep(*.is-prime)[^5].join(','), '2,3,5,7,11', 'method grep is-prime on 1..Inf';

# Sub form
is grep(* %% 2, 1..Inf)[^3].join(','), '2,4,6', 'sub grep on 1..Inf, subscript ^3';
is grep(*.is-prime, 1..Inf)[^5].join(','), '2,3,5,7,11', 'sub grep is-prime on 1..Inf';

# Finite ranges must be unchanged (no regression)
is (1..10).grep(* %% 2).join(','), '2,4,6,8,10', 'finite method grep';
is grep(* %% 2, 1..10).join(','), '2,4,6,8,10', 'finite sub grep';

# Exclusive range endpoints
is (1..^10).grep(* %% 2).join(','), '2,4,6,8', 'half-open range grep';
is (1^..^10).grep(* %% 2).join(','), '2,4,6,8', 'open range grep';

# :k adverb on finite range
is (1..10).grep(* %% 2, :k).join(','), '1,3,5,7,9', 'grep :k indices on finite range';

done-testing;
