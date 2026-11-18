use Test;

# A role declared after mainline code is registered early as a "shell" so the
# code before it can reach it. That shell must not run the role's method
# traits: they run once, at the real declaration. Running them from the shell
# too ran each trait twice, and a HOW role a trait mixed in (Method::Also's
# `is also<...>`, whose role-side `specialize` mixes a `compose` hook into the
# consuming class's HOW) reached the class before its real declaration, which
# then treated the hook as pre-existing and never ran it.

plan 5;

my @seen;
my %aliases;

my role AliasClassHOW {
    method compose(Mu \o, :$compiler_services) {
        @seen.push: "compose " ~ o.^name;
        for %aliases{o.^name}[] { o.^add_method(.key, .value) if $_ }
        nextsame;
    }
}

my role AliasRoleHOW {
    method specialize(Mu \r, Mu:U \obj, *@pos, *%named) {
        obj.HOW does AliasClassHOW unless obj.HOW ~~ AliasClassHOW;
        callsame;
    }
}

multi sub trait_mod:<is>(Method:D \meth, :$also!) {
    @seen.push: "trait " ~ meth.name ~ " in " ~ $*PACKAGE.^name;
    if $*PACKAGE.HOW ~~ Metamodel::ClassHOW {
        $*PACKAGE.HOW does AliasClassHOW unless $*PACKAGE.HOW ~~ AliasClassHOW;
    }
    if $*PACKAGE.HOW ~~ Metamodel::ParametricRoleHOW {
        $*PACKAGE.HOW does AliasRoleHOW unless $*PACKAGE.HOW ~~ AliasRoleHOW;
    }
    %aliases{$*PACKAGE.^name}.push: Pair.new(.Str, meth) for @$also;
}

# Mainline code before the declarations makes them hoisted shells.
my $before = 1;

role Ber {
    proto method bar_ber(|) is also<bar-ber> {*}
    multi method bar_ber(Str $) { 'Str' }
}

class Bar does Ber {
    multi method foo()     is also<bar>   { 42 }
    multi method foo($foo) is also<bazzy> { $foo }
}

is @seen.grep(*.starts-with('trait bar_ber')).elems, 1, 'the role method trait runs once';
is @seen.grep(*.starts-with('trait foo')).elems, 2, 'each class method trait runs once';
is @seen.grep('compose Bar').elems, 1, 'the class HOW compose hook runs once';
is Bar.bar, 42, 'the alias the compose hook installed is callable';
is Bar.bazzy(7), 7, 'the second alias too';
