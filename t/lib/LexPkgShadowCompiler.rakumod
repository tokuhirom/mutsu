# EVALs code from inside a loaded module's routine, as Red's migration
# compiler does (#18409).
unit module LexPkgShadowCompiler;
sub compile-it(Str $code) is export {
    use MONKEY-SEE-NO-EVAL;
    EVAL $code
}
