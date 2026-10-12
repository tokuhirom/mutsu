use Test;

# An error thrown inside a BEGIN phaser is wrapped in X::Comp::BeginTime (BEGIN
# runs at compile time). The original exception is carried as `.exception`.

plan 5;

throws-like 'BEGIN { die "oh noes!" }', X::Comp::BeginTime,
    exception => sub ($e) { $e.message eq 'oh noes!' },
    'die in BEGIN is X::Comp::BeginTime with the original exception';

# NOTE: real `raku` surfaces a Rat's lazy div-by-zero as plain
# X::Numeric::DivideByZero when `.Int` is the BEGIN block's *value* (the check
# escapes the BEGIN-wrapping catch frame), and mutsu matches that for a unit
# ending in such a BEGIN (#12900). With the failing call in sink position the
# error is wrapped under `raku` and in mutsu alike, which is what this pins.
throws-like 'BEGIN { my $x = 1 / 0; $x.Int; 5 }', X::Comp::BeginTime,
    'a numeric failure in BEGIN wraps too';

# Normal BEGIN behaviour is preserved: side effects run, and a BEGIN value is usable.
my $log = '';
EVAL 'BEGIN { $*OUT.print: "" }';  # smoke: BEGIN parses/runs
is (BEGIN { 21 * 2 }), 42, 'BEGIN as an rvalue yields its value';
lives-ok { EVAL 'BEGIN { 1 + 1 }' }, 'a non-throwing BEGIN lives';

# INIT (runtime phaser) errors are NOT wrapped as BeginTime.
throws-like 'INIT { die "later" }', X::AdHoc,
    'die in INIT is a plain runtime error, not BeginTime';
