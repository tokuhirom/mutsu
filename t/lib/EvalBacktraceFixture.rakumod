# Used by t/exceptions/eval-unit-backtrace-frame.t: an EVAL run from inside a
# module routine, the shape of Rakuish's REPL session.
unit class EvalBacktraceFixture;

method run(Str $code) {
    my $ex;
    {
        EVAL $code;
        CATCH { default { $ex = $_ } }
    }
    $ex
}
