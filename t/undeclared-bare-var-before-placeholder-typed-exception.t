use MONKEY-SEE-NO-EVAL;
use Test;

# A bare `$name` used before its own `$^name` placeholder in the same block
# (or a `$name` shadowed only by a nested block's `$^name`) is a compile-time
# X::Undeclared error. The compiler used to build this as a plain string
# (`"X::Undeclared: ..."`) rather than a real X::Undeclared instance, so the
# raised value's actual class was X::AdHoc — invisible to mutsu's native,
# lenient `throws-like` (which matches on message text), but wrong once the
# real vendored Test.rakumod's strict `throws-like` (`.^name` check) drives
# it. Both branches must build a genuine typed instance, with the exact
# message `raku` reports (verified directly against `raku`).

plan 4;

{
    my $ex;
    try { EVAL 'my $f = { say $b; say $^b }; $f(1)' }
    $ex = $!;
    isa-ok $ex, X::Undeclared,
        'bare $b before its own $^b in a block is a real X::Undeclared instance';
    is $ex.message,
        "Variable '\$b' is not declared. Perhaps you forgot a 'sub' if this was\nintended to be part of a signature?",
        'message matches raku exactly';
}

{
    my $ex;
    # The immediate-call form `{ ... }()` is what actually routes through the
    # nested-placeholder-shadow branch at compile time (a bare `{ ... }`
    # statement without its own top-level placeholder skips the check
    # entirely, so it falls to the unrelated, already-correctly-typed runtime
    # `strict_undeclared_error` path instead).
    try { EVAL '{ for 1 { $^b }; say $b }()' }
    $ex = $!;
    isa-ok $ex, X::Undeclared,
        'bare $b shadowed only by a nested block\'s $^b is a real X::Undeclared instance';
    is $ex.message,
        "Variable '\$b' is not declared. Perhaps you forgot a 'sub' if this was\nintended to be part of a signature?",
        'message matches raku exactly';
}
