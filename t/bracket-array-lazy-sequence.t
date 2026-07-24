use v6;
use Test;

# A `[...]` bracket-array literal wrapping a single genuinely-lazy list keeps
# the array lazy, exactly as `my @a = 1,2,3...*` does: `.is-lazy` is True and a
# strict operation like `.elems` throws X::Cannot::Lazy instead of eagerly
# materializing the infinite sequence (which would hang or truncate to the seed
# cache). Previously mutsu reified `[1,2,3...*]` / `[-Inf...Inf]` eagerly.

# --- Infinite arithmetic `...` sequence stays lazy ---
ok [1,2,3...*].is-lazy, "[arithmetic ...* sequence] is lazy";
throws-like { [1,2,3...*].elems }, X::Cannot::Lazy,
    "[1,2,3...*].elems throws X::Cannot::Lazy";
is [1,2,3...*][^5].join(","), "1,2,3,4,5", "[1,2,3...*] indexes lazily";
is [1,2,3...*].head(3).join(","), "1,2,3", "[1,2,3...*].head pulls a bounded prefix";

# --- Infinite geometric `...` sequence stays lazy ---
ok [1,2,4...*].is-lazy, "[geometric ...* sequence] is lazy";
is [1,2,4...*][^6].join(","), "1,2,4,8,16,32", "[1,2,4...*] indexes lazily";

# --- The `[-Inf...Inf]` doc example (Type/Array) ---
{
    try [-Inf...Inf].elems;
    is $!.^name, "X::Cannot::Lazy", "[-Inf...Inf].elems throws X::Cannot::Lazy";
}
ok [-Inf...Inf].is-lazy, "[-Inf...Inf] is lazy";

# --- A lazy map/grep pipe over an infinite source stays lazy ---
ok [(1..*).map(* + 1)].is-lazy, "[lazy pipe over infinite range] is lazy";
is [(1..*).map(* * 2)][^4].join(","), "2,4,6,8", "[lazy pipe] indexes lazily";

# --- Regression: finite bracket arrays still materialize eagerly ---
is [1,2,3].elems, 3, "finite bracket array materializes";
is [1..5].elems, 5, "finite range bracket array materializes";
is [2..6].join(","), "2,3,4,5,6", "finite range flattens";
is [1,2,3...10].join(","), "1,2,3,4,5,6,7,8,9,10", "finite ...N sequence materializes";
is [1,3...11].join(","), "1,3,5,7,9,11", "finite step ...N sequence materializes";
is [(1,2,3)].elems, 3, "single list element flattens";

# --- Regression: a plain gather bracket array is eager (finite, not lazy) ---
nok [gather { take 1; take 2 }].is-lazy, "[plain gather] is not lazy";
is [gather { take 1; take 2 }].elems, 2, "[plain gather] materializes";

# --- Regression: a `lazy`-marked *finite* bracket array is NOT kept lazy here
#     (only infinite lists are), so whole-array `cmp` reads every element
#     (roast S03-operators/cmp.t "lazy array comparisons"). ---
is-deeply ([lazy 1, 2] cmp [lazy 1, 2, 3]), Less, "[lazy finite] cmp: shorter is Less";
is-deeply ([lazy 1, 3] cmp [lazy 1, 2]),    More, "[lazy finite] cmp: differ at pos 1";

# --- Assignment from a lazy bracket array is consistent ---
{
    my @a = [1,2,3...*];
    ok @a.is-lazy, "my @a = [1,2,3...*] stays lazy";
    is @a[^5].join(","), "1,2,3,4,5", "assigned lazy array indexes lazily";
}

done-testing;
