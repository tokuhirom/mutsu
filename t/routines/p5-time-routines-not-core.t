use Test;

# `times`, `localtime` and `gmtime` are Perl 5 routines, not Raku core ones
# (rakudo: "Undeclared routine"). They used to be mutsu builtins, and `times`
# was even parsed as a hard-coded 0-arg term, so a user/module `times`
# (P5times) was shadowed: `times(Scalar)` became `times()` followed by a
# postfix call on its List result. See issue #9418.

plan 7;

{
    my proto sub times(|) {*}
    multi sub times(Scalar:U) { 42 }
    multi sub times() { (1, 2) }
    is times(Scalar), 42, 'user times(Scalar) dispatches to the user multi';
    is-deeply times, (1, 2), 'bare user times calls the nullary user multi';
}

{
    sub localtime($x?) { "local:" ~ ($x // 'none') }
    sub gmtime($x?)    { "gm:" ~ ($x // 'none') }
    is localtime(0), 'local:0', 'user localtime is not shadowed';
    is gmtime(), 'gm:none', 'user gmtime is not shadowed';
}

for <times localtime gmtime> -> $name {
    throws-like $name ~ "()", X::Undeclared::Symbols,
        "$name is not a core routine";
}
