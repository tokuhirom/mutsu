class EvalCtxSelfRef {
    method tag() { 'self-ref' }
}

# Both of these name this module's OWN package, qualified. Each is checked by
# the #7797 qualified-name visibility gate at the moment it runs, so each needs
# the compunit it is judged against to be this file — which it is not by
# default when the module is pulled in by an `EVAL "use ..."` reached from
# another module's routine, nor (for the END) at program exit.
sub EXPORT($name = 'ctx-self-ref') {
    %( $name => EvalCtxSelfRef.new )
}

END {
    # No output: the point is that this resolves at all. Before #7837 it died
    # with "Could not find symbol 'EvalCtxSelfRef'" and took the exit status
    # with it.
    my $ = EvalCtxSelfRef.new.tag;
}
