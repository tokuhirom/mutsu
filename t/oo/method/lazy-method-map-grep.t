use Test;

# Method-form `.map` / `.grep` over an infinite source must stay truly lazy:
# they return a lazy Seq and never materialize the (infinite) source. Before
# this was implemented, `(1..Inf).map(...)` eagerly expanded the range to a 1M
# cap (slow, wrong `.elems`/`.is-lazy`) and chained `.map(...).grep(...)` could
# take tens of seconds. See PLAN.md §8.1 (pull-model unification).

plan 22;

# --- map over an infinite range stays lazy --------------------------------
is (1..Inf).map(* * 2).head(3).join(","), "2,4,6", "map.head(3)";
is (1..Inf).map(* * 2)[3], 8, "map[3]";
is (1..Inf).map(* * 2)[^4].join(","), "2,4,6,8", "map[^4]";
is (1..Inf).map(* * 2).first(* > 10), 12, "map.first";
ok (1..Inf).map(* * 2).is-lazy, "map result is-lazy";
is (1..Inf).map(* * 2).WHAT.^name, "Seq", "map returns a Seq";

# --- grep over an infinite range stays lazy -------------------------------
is (1..Inf).grep(* %% 2).head(3).join(","), "2,4,6", "grep.head(3)";
is (1..Inf).grep(* %% 2)[^3].join(","), "2,4,6", "grep[^3]";
is (1..Inf).grep(* %% 3).first(* > 10), 12, "grep.first";
is (1..Inf).grep(*.is-prime).head(5).join(","), "2,3,5,7,11", "grep prime";
ok (1..Inf).grep(* %% 2).is-lazy, "grep result is-lazy";

# --- chained lazy stages --------------------------------------------------
is (1..Inf).map(* * 2).grep(* %% 4).head(3).join(","), "4,8,12", "map.grep.head";
is (1..Inf).grep(*.is-prime).map(* + 1).head(3).join(","), "3,4,6", "grep.map.head";
is (1..Inf).map(* * 2).grep(* %% 4).map(* + 1).head(3).join(","), "5,9,13", "map.grep.map.head";
is (1..Inf).map(* * 2).grep(* %% 4)[^3].join(","), "4,8,12", "chain[^3]";
is (1..Inf).map(* * 2).grep(* %% 4).first(* > 20), 24, "chain.first";
ok (1..Inf).map(* * 2).grep(* %% 4).is-lazy, "chained result is-lazy";

# --- lazy iteration via for-loop ------------------------------------------
my @seen;
for (1..Inf).map(* * 2) { @seen.push($_); last if $_ >= 6 }
is @seen.join(","), "2,4,6", "for over lazy map";

my @seen2;
for (1..Inf).map(* * 2).grep(* %% 4) { @seen2.push($_); last if $_ >= 12 }
is @seen2.join(","), "4,8,12", "for over lazy chain";

# --- strict (whole-list) ops on an infinite lazy pipe throw X::Cannot::Lazy
throws-like { (1..Inf).map(* * 2).elems }, X::Cannot::Lazy, "elems throws";
throws-like { (1..Inf).map(* * 2).sort }, X::Cannot::Lazy, "sort throws";

# --- finite sources keep their eager behaviour (no regression) ------------
is (1..5).map(* * 2).join(","), "2,4,6,8,10", "finite map unchanged";
