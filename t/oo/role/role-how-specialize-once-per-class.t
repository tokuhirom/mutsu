use Test;

# A role's HOW `specialize` override runs once per class that composes the
# role, at the class's real declaration. A forward-reference shell of the class
# (registered before the program reaches it) also ran it. A hook that mixes a
# `compose` override into the class HOW then counted as present before the
# class's own method traits ran, so the class never ran that compose. This is
# Method::Also's shape (`is also<alias>` on a method of a class that does a role
# with its own alias), and it made `Bar.bar` "No such method". Expected values
# checked against raku.

plan 3;

my %aliases;
my @specialized;
my @composed;

my role AliasClassHOW {
    method compose(Mu \o, :$compiler_services) {
        @composed.push: o.^name;
        for %aliases{o.^name}[] { o.^add_method(.key, .value) if $_ }
        nextsame;
    }
}
my role AliasRoleHOW {
    method specialize(Mu \r, Mu:U \obj, *@pos, *%named) {
        @specialized.push: r.^name ~ '>' ~ obj.^name;
        obj.HOW does AliasClassHOW unless obj.HOW ~~ AliasClassHOW;
        callsame;
    }
}
multi sub trait_mod:<is>(Method:D \meth, :$also!) {
    $*PACKAGE.HOW does AliasClassHOW
        if $*PACKAGE.HOW ~~ Metamodel::ClassHOW && !($*PACKAGE.HOW ~~ AliasClassHOW);
    $*PACKAGE.HOW does AliasRoleHOW
        if $*PACKAGE.HOW ~~ Metamodel::ParametricRoleHOW && !($*PACKAGE.HOW ~~ AliasRoleHOW);
    %aliases{$*PACKAGE.^name}.push: Pair.new(.Str, meth) for @$also;
}

role Ber { method x is also<y> { 1 } }
class Bar does Ber {
    multi method foo() is also<bar> { 42 }
}

is Bar.bar, 42, "the class's own alias is installed by its HOW's compose";
is-deeply @specialized, ['Ber>Bar'], "the role's specialize ran once";
is-deeply @composed, ['Bar'], "the class HOW's compose ran once";
