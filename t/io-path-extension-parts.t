use Test;

plan 6;

is "foo.txt.tar.gz".IO.extension(:2parts), "tar.gz", "Int :parts extracts multiple suffix parts";
is "foo.txt.tar.gz".IO.extension(:parts(0..^Inf)), "txt.tar.gz", "Range :parts picks maximal match";
is "foo.txt.tar.gz".IO.extension("BAR", :parts(0..3), :joiner("_")).Str, "foo_BAR", "replacement honors :joiner";
is "...".IO.extension("tar").Str, "...tar", "replacement keeps dotted basename edge case";

sub prefix:<pIO> ($p) is looser(&[~]) { $p.IO }
is (pIO "foo" ~ "_BAR").Str, "foo_BAR", "looser prefix sub consumes full RHS expression";
is (pIO "foo" ~ "_BAR"), "foo_BAR".IO, "looser prefix sub returns IO::Path";
