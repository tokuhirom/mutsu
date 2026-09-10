use Test;

# Eager list operations that need the full (finite) list must throw
# X::Cannot::Lazy on a lazy/infinite source instead of hanging while
# materializing it (PLAN.md §8.1 / §8.2, matches raku). `.head`/`.first`/`.map`
# stay lazy and are NOT affected.

plan 18;

# --- method form on infinite Range: throws X::Cannot::Lazy ---
throws-like { (1..Inf).sort },         X::Cannot::Lazy, 'method sort on 1..Inf';
throws-like { (1..Inf).combinations }, X::Cannot::Lazy, 'method combinations on 1..Inf';
throws-like { (1..Inf).permutations }, X::Cannot::Lazy, 'method permutations on 1..Inf';
throws-like { (1..Inf).classify(* %% 2) },   X::Cannot::Lazy, 'method classify on 1..Inf';
throws-like { (1..Inf).categorize(* %% 2) }, X::Cannot::Lazy, 'method categorize on 1..Inf';

# --- infinite sequence (LazyList) ---
throws-like { (1, 2, 4 ... Inf).sort }, X::Cannot::Lazy, 'method sort on infinite sequence';

# --- sub form ---
throws-like { sort(1..Inf) },         X::Cannot::Lazy, 'sub sort on 1..Inf';
throws-like { combinations(1..Inf) }, X::Cannot::Lazy, 'sub combinations on 1..Inf';
throws-like { permutations(1..Inf) }, X::Cannot::Lazy, 'sub permutations on 1..Inf';

# --- finite inputs are completely unaffected (no regression) ---
is (3, 1, 2).sort.join(','), '1,2,3', 'finite method sort';
is sort(3, 1, 2).join(','), '1,2,3', 'finite sub sort';
is (1..3).combinations.elems, 8, 'finite combinations';
is (1..3).permutations.elems, 6, 'finite permutations';
is (1..6).classify(* %% 2){True}.join(','), '2,4,6', 'finite classify';
is <c a b>.sort.join(','), 'a,b,c', 'finite string sort';

# --- finite-but-lazy-origin sources still work (gather/map/Seq force fine) ---
is (gather { take 3; take 1; take 2 }).sort.join(','), '1,2,3', 'finite gather sort';
is (1..5).map(* * 2).sort.join(','), '2,4,6,8,10', 'finite lazy-map sort';
is (1, 2, 3 ... 6).sort.join(','), '1,2,3,4,5,6', 'finite sequence sort';

done-testing;
