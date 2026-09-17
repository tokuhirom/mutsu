use Test;

# A CONTROL block always *runs* when a `return` unwinds through its scope,
# but it only *handles* that return when one of its `when`/`default` clauses
# actually matches the signal (same rule already applied to next/last/warn/
# etc. — see t/exceptions/control.t). An unmatched CONTROL block must let
# the return keep its original value; it must not silently turn it into
# Nil just because a CONTROL block happened to be in lexical scope.
#
# Found via the ecosystem XDG::GuaranteedResources / File::Directory::Tree
# distributions: File::Directory::Tree's `guarantee-resource` (etc.) return
# a value from inside a sub whose body also declares a CONTROL block (for an
# unrelated CX::Warn), and every one of those subs returned Nil under mutsu.
plan 4;

sub returns-through-unmatched-control() {
    my $x = "hi";
    return $x;
    CONTROL {
        when CX::Warn { die "warn: $_"; }
    }
}
is returns-through-unmatched-control(), "hi",
    "a return whose CONTROL block matches nothing keeps its value";

sub returns-through-matched-control() {
    my $x = "hi";
    return $x;
    CONTROL {
        when CX::Return { "matched" }
    }
}
is returns-through-matched-control(), Nil,
    "a return whose CONTROL block DOES match is absorbed (like rakudo)";

# The same rule inside a bare block (no enclosing sub signature machinery).
sub via-bare-block() {
    my $r = do {
        my $y = "inner";
        return $y;
        CONTROL {
            when CX::Warn { die "warn"; }
        }
    };
    return "never: $r";
}
is via-bare-block(), "inner",
    "return from inside a block with an unmatched CONTROL keeps its value";

# A `return` that is genuinely dead by the time it reaches an unmatched
# CONTROL block (its target routine frame already exited) must still
# surface as `X::ControlFlow::Return`, not silently vanish either.
sub makes-a-dead-return() {
    my &closure;
    sub make() {
        &closure = -> { return "from-outer" };
        Nil;
    }
    make();
    &closure();
    CONTROL {
        when CX::Warn { die "warn"; }
    }
}
dies-ok { makes-a-dead-return() },
    "a dead return past an unmatched CONTROL still throws X::ControlFlow::Return";
