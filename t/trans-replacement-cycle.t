use v6;
use Test;

# `.trans` has two forms with different short-replacement semantics:
#   * Str => Str (the first multi): the replacement is CYCLED to the key length.
#   * list/range key or value (the second multi): the last replacement char is
#     repeated, and `:delete` drops the unmatched trailing chars.

plan 12;

# Str => Str cycles a short replacement (doc example).
is "a123b123c".trans('123' => 'þð'), "aþðþbþðþc", "Str=>Str cycles the replacement";
is "12345".trans('12345' => 'AB'), "ABABA", "Str=>Str cycle over a longer key";
is "1234567".trans('1234567' => 'ABC'), "ABCABCA", "Str=>Str cycle with a 3-char replacement";

# A Str key with a list/range target is dispatched to the list form: repeat-last.
is "12345".trans('12345' => (1, 2)), "12222", "Str key + list value repeats the last char";
is "12345".trans('12345' => 1..2), "12222", "Str key + range value repeats the last char";

# A Seq key (e.g. from `.comb`) is list-like, not a stringified sequence.
is "abc".trans("abc".comb => 1..2, :delete), "12", ".comb key + :delete drops the unmatched tail";
is "abc".trans("abc".comb => ("1", "2")), "122", ".comb key repeats the last replacement";

# List key with an integer/range value works the same as a string list value.
is "abc".trans(<a b c> => (1, 2), :delete), "12", "list key + int list value + :delete";

# `:squash` and `:delete` make the Str=>Str form a strict substitution (doc).
is "a123b123c".trans('123' => 'þð', :squash), "aþðbþðc", ":squash strict-substitutes like :delete";
is "a123b123c".trans('123' => 'þð', :delete), "aþðbþðc", ":delete strict-substitutes";

# Equal-length Str=>Str is unaffected.
is "a123b123c".trans('23' => '4'), "a144b144c", "equal-length map is a plain substitution";

# Range key => range value (list form) with :squash still works (doc example).
is "aaa1123bb123c".trans('a'..'z' => 'A'..'Z', :squash), "A1123B123C", "range=>range :squash";

done-testing;
