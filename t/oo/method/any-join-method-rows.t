use Test;
plan 20;

# Any.join is self.list.join (ADR-11276 §9.63); expectations checked against raku.
my %h = a => 1;
is %h.join, "a\t1", 'Hash.join';
is %h.join("-"), "a\t1", 'Hash.join with a separator';
is Map.new((a => 1)).join("|"), "a\t1", 'Map.join';
is (a => 1).join, "a\t1", 'Pair.join';
is (a => 1).join("-"), "a\t1", 'Pair.join never shows the separator';
is (1..5).join, "12345", 'Range.join';
is (1..5).join("-"), "1-2-3-4-5", 'Range.join with a separator';
is \(1, 2).join, "12", 'Capture.join';
is \(1, 2, :a(3)).join("-"), "1-2", 'Capture.join reads the positionals';
"abcd" ~~ /(..)(..)/;
is $/.join, "abcd", 'Match.join';
is $/.join("-"), "ab-cd", 'Match.join joins the captures';
is 5.join, "5", 'Int.join';
is 5.join("x"), "5", 'Int.join with a separator';
is "ab".join("x"), "ab", 'Str.join';
is 1.5.join, "1.5", 'Rat.join';
is True.join, "True", 'Bool.join';
is 'ba'.NFC.join, "9897", 'Uni.join';
is 'ba'.NFC.join(","), "98,97", 'Uni.join with a separator';
is Date.new("2020-01-02").join, "2020-01-02", 'Date.join';
is (1, 2, 3).join("-"), "1-2-3", 'List.join still answers';
