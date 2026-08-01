use Test;

# A pragma the EVAL'd unit turns on is scoped to that unit. `use fatal` is the
# one that bites, because mutsu keeps it as an interpreter-wide flag: without a
# save/restore around EVAL the *caller* went on throwing for every later soft
# Failure. `throws-like 'use fatal; ...'` is a common assertion shape, so one of
# them poisoned the rest of the file.

plan 6;

use MONKEY-SEE-NO-EVAL;

# Inside the EVAL'd unit the pragma is in force.
{
    try { EVAL q{use fatal; "foo"[2]} };
    isa-ok $!, X::OutOfRange, 'use fatal inside the EVAL still throws there';
}

# Outside it is not.
{
    my $f = "bar"[5];
    isa-ok $f, Failure, 'the caller keeps its soft Failure afterwards';
    ok $f.defined.not, 'and it is undefined, not thrown';
}

# The same when the EVAL merely turns the pragma on.
{
    EVAL q{use fatal};
    my $f = "bar"[5];
    isa-ok $f, Failure, 'a pragma-only EVAL does not leak either';
}

# Nested: the inner EVAL's pragma does not escape into the outer one.
{
    EVAL q{use MONKEY-SEE-NO-EVAL; EVAL q[use fatal]; };
    my $f = "bar"[5];
    isa-ok $f, Failure, 'nor does a nested EVAL leak it outward';
}

# LAST, because it turns the pragma on for the rest of the file: the caller's
# own `use fatal` is untouched by an EVAL that does not set it.
use fatal;
EVAL q{1 + 1};
throws-like { my $f = "bar"[5]; $f }, X::OutOfRange,
    'an EVAL does not clear the caller\'s own use fatal';
