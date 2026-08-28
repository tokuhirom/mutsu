unit module PackageTypeShortName;

use MONKEY-SEE-NO-EVAL;

# A type declared inside a package is registered under its QUALIFIED name only,
# so every bareword reference to its SHORT name is a reference that has to be
# resolved through the running package's chain. The declaration `my Modish $Modish`
# is the stress case: the scalar and the type share the sigil-stripped lexical
# key, so the variable must not be mistaken for the type (and vice versa).

class Modish { has $.a }

# A same-named typed declaration at module scope, outside any routine.
my Modish $Modish;

sub module-scope-decl-name() is export { $Modish.^name }

sub module-scope-bareword-name() is export { Modish.^name }

sub in-module-sub() is export {
    class Subbish { has $.a }
    my Subbish $Subbish .= new(:a(7));
    ($Subbish.a, $Subbish.^name, Subbish.^name)
}

# `EVAL` called from a sub of a separate compilation unit: the snippet's own
# `class` lands in this module's package, which is exactly where the short-name
# resolution used to be lost.
sub eval-in-module($code) is export { EVAL $code }

sub eval-exception($code) is export {
    try { EVAL $code }
    $!
}
