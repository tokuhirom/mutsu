unit module EvalCtxHelper;
use MONKEY-SEE-NO-EVAL;

# The shape the vendored `Test.rakumod` uses for the string form of
# `throws-like`: capture the caller's context, then compile the snippet in it.
# The snippet must see what the *test file* imported, not what this module did.
sub eval-in-caller(Str $code) is export {
    my $caller-context = CALLER::;
    EVAL $code, context => $caller-context;
}

# The same EVAL without the `context` argument, for contrast: this one really
# does compile in this module's scope, where nothing the caller imported is
# visible (rakudo agrees).
sub eval-no-context(Str $code) is export {
    EVAL $code;
}

# The shape `Test.rakumod`'s `use-ok` uses: an `EVAL "use ..."` reached from
# inside a routine of another compunit.
sub eval-use(Str $module) is export {
    EVAL "use $module";
    'loaded'
}
