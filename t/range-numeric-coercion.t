use Test;

# A Range numerifies to its element count (.elems): .Int/.Numeric/.Real yield
# that count as an Int, .Num as a Num. An infinite range yields Inf for the
# real-valued coercions and fails for .Int. Previously these all errored with
# "No such method ... for invocant of type 'Range'".

plan 22;

# --- finite integer ranges ---
is (1..10).Numeric, 10, "Range.Numeric is the element count";
is (1..10).Int, 10, "Range.Int is the element count";
is (1..10).Real, 10, "Range.Real is the element count";
is (1..10).Num, 10, "Range.Num is the element count";
is (1..10).Numeric.^name, "Int", "Range.Numeric returns an Int";
is (1..10).Int.^name, "Int", "Range.Int returns an Int";
is (1..10).Real.^name, "Int", "Range.Real returns an Int";
is (1..10).Num.^name, "Num", "Range.Num returns a Num";

# --- exclusive ranges ---
is (0..^10).Int, 10, "exclusive-end Range.Int";
is (0^..10).Int, 10, "exclusive-start Range.Int";
is (0^..^10).Int, 9, "exclusive-both Range.Int";

# --- non-integer-endpoint ranges ---
is (1.5..5.5).Int, 5, "Rat-endpoint Range.Int";
is (1.5..5.5).Numeric, 5, "Rat-endpoint Range.Numeric";
is ("a".."e").Numeric, 5, "Str Range.Numeric";

# --- consistency with .elems and numeric context ---
is (1..10).Int, (1..10).elems, "Range.Int equals .elems";
is +(1..10), 10, "Range in numeric context is its element count";

# --- infinite ranges ---
is (1..Inf).Numeric, Inf, "infinite Range.Numeric is Inf";
is (1..Inf).Real, Inf, "infinite Range.Real is Inf";
is (1..Inf).Num, Inf, "infinite Range.Num is Inf";
is (-Inf..0).Numeric, Inf, "left-infinite Range.Numeric is Inf";
dies-ok { (1..Inf).Int }, "infinite Range.Int dies (cannot convert Inf to Int)";

# --- regression: non-Range coercions still work ---
is (1, 2, 3).Numeric, 3, "List.Numeric unaffected";
