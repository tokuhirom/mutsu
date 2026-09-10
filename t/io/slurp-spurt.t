use Test;
plan 5;

# Basic roundtrip
spurt("_test_slurp_spurt.txt", "hello world");
is slurp("_test_slurp_spurt.txt"), "hello world", "slurp/spurt roundtrip";

# Overwrite existing content
spurt("_test_slurp_spurt.txt", "new content");
is slurp("_test_slurp_spurt.txt"), "new content", "spurt overwrites existing";

# Unicode content
spurt("_test_slurp_spurt.txt", "cafe\x[301]");
is slurp("_test_slurp_spurt.txt"), "cafe\x[301]", "Unicode roundtrip";

# slurp non-existent file dies
dies-ok { my $r = slurp("_no_such_file_12345.txt") }, "slurp non-existent file dies";

# Cleanup
unlink("_test_slurp_spurt.txt");
ok True, "unlink succeeds";
