use Test;

# A class's roles are applied (a role HOW's `specialize` called) by
# `Metamodel::ClassHOW.compose`, so a user `compose` override on the class's
# HOW runs first and the `specialize` happens inside its `nextsame` (#18145).

plan 3;

my @log;
my role CH {
    method compose(Mu \o, :$compiler_services) {
        @log.push: "compose {o.^name}";
        nextsame;
    }
}
my role RH {
    method specialize(Mu \r, Mu:U \obj, *@p, *%n) {
        @log.push: "specialize {r.^name}";
        callsame;
    }
}
multi sub trait_mod:<is>(Method:D \m, :$x!) {
    $*PACKAGE.HOW does CH if $*PACKAGE.HOW ~~ Metamodel::ClassHOW && $*PACKAGE.HOW !~~ CH;
    $*PACKAGE.HOW does RH if $*PACKAGE.HOW ~~ Metamodel::ParametricRoleHOW && $*PACKAGE.HOW !~~ RH;
}
role R { method r is x { 'r' } }
class C does R { method c is x { 'c' } }

is @log.join(' | '), 'compose C | specialize R', 'the compose override runs, then the role specializes';
is C.new.r, 'r', 'the role is composed';
is C.new.c, 'c', 'and so is the class';
