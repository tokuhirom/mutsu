use Test;

plan 9;

# An untyped runtime error (one that mutsu raises without an attached typed
# exception) surfaces as X::AdHoc -- the class a bare `die "msg"` produces in
# Raku -- not the abstract base Exception. X::AdHoc IS-A Exception, so the
# Exception-level checks must keep matching too.

# Bare string die.
{
    my $ex;
    try { die "boom"; CATCH { default { $ex = $_ } } };
    isa-ok $ex, X::AdHoc, 'die "str" -> X::AdHoc';
    isa-ok $ex, Exception, 'X::AdHoc IS-A Exception';
    is $ex.message, 'boom', 'message preserved';
}

# Failure.new with no explicit exception.
{
    my $f = Failure.new;
    isa-ok $f.exception, X::AdHoc, 'Failure.new default exception is X::AdHoc';
}

# A POST-style / phaser-visible $! also carries X::AdHoc.
{
    my $ex;
    try {
        die "in-block";
        CATCH { default { $ex = $! } }
    };
    isa-ok $ex, X::AdHoc, '$! from untyped die is X::AdHoc';
}

# A *typed* error keeps its specific type (regression guard: the default must
# only apply when no typed exception is attached).
{
    my $ex;
    try { my Int $x = "nope"; CATCH { default { $ex = $_ } } };
    isa-ok $ex, X::TypeCheck::Assignment, 'typed assignment error keeps its type';
    nok $ex.isa(X::AdHoc), 'typed error is not downgraded to X::AdHoc';
}

# throws-like at the Exception level still matches an untyped die.
throws-like { die "x" }, Exception, 'throws-like Exception matches untyped die';
throws-like { die "x" }, X::AdHoc, 'throws-like X::AdHoc matches untyped die';
