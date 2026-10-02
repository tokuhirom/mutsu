use Test;

# Routine.add_dispatchee on a proto, as used by JSON::Fast::Hyper
# (ecosystem CLI::Ecosystem dependency closure).
plan 3;

my proto sub p(|) {*}
my multi sub p(Int $x) { "int $x" }
sub other(Str $s) { "str $s" }

BEGIN &p.add_dispatchee(&other);
is p(1), "int 1", "existing candidate still dispatches";
is p("a"), "str a", "added dispatchee is selected by signature";

throws-like { &p.add_dispatchee(42) }, Exception, "non-routine argument is rejected";
