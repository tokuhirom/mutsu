use MONKEY-SEE-NO-EVAL;
unit module EvalContext;

sub run-plain($code) is export {
    EVAL $code
}

sub run-with-caller-context($code) is export {
    my $ctx = CALLER::;
    EVAL $code, context => $ctx
}

# The context is taken here but used several frames deeper, inside a block --
# the shape rakudo's `Test.rakumod` uses for the string form of `throws-like`.
sub run-context-through-block($code) is export {
    my $ctx = CALLER::;
    my $inner = -> { EVAL $code, context => $ctx };
    $inner()
}
