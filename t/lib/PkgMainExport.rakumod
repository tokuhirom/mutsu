unit module PkgMainExport;

# A module that exports a MAIN family (App::Stouch's shape).
our proto MAIN(|) is export {*};
multi sub MAIN(Int $x) { "int $x" }
multi sub MAIN(Str $x, :$d = 'dflt') { "str $x $d" }
