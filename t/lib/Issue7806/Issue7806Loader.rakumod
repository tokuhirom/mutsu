unit module Issue7806Loader;
use MONKEY-SEE-NO-EVAL;

# Shaped exactly like `Test`'s own `use-ok`: `EVAL ( "use $code" )` inside a
# `try { }` inside a sub, so the module's first load happens from inside a
# nested call frame whose own env overlay is discarded on return.
sub issue7806-use-ok(Str $code) is export {
    try {
        EVAL ( "use $code" );
    }
}
