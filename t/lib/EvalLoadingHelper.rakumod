use MONKEY-SEE-NO-EVAL;
unit module EvalLoadingHelper;

# The shape of `Test`'s own `use-ok`: a module routine that loads another module
# through a string EVAL. The load therefore runs with THIS module's routine
# frame still on the stack, which is what used to misattribute every `sub` the
# loaded module declares to this compunit (#7836).
our sub load-via-eval(Str $module) is export {
    try {
        EVAL ( "use $module" );
    }
    $! ?? "FAILED: $!.Str()" !! 'ok'
}
