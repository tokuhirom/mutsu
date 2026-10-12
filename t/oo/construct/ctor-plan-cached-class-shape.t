use Test;

# The class-shape probes of a construction (role BUILD/TWEAK submethods,
# `:D`/`:U` smileys, `is required`, a QuantHash base) are answered from the
# class's cached constructor plan (#12917). Every construction after the
# first takes the cached answer, so each check runs on several objects.

plan 15;

my @log;
role B { has $.x; submethod BUILD(:$!x = 1) { @log.push: "B.BUILD" } }
class CB does B { }
role S { has Str:D $.s is required; submethod TWEAK { @log.push: "S.TWEAK" } }
class C does S { has Int:D $.n = 0 }

for ^2 {
    @log = ();
    my $c = CB.new(:x(5));
    is "{@log.join(',')} {$c.x}", 'B.BUILD 5', "a role BUILD runs (construction $_)";
}
for ^2 {
    @log = ();
    my $c = C.new(:s<a>, :n(2));
    is "{@log.join(',')} {$c.s}{$c.n}", 'S.TWEAK a2', "a composed role's TWEAK runs (construction $_)";
}

for ^2 {
    throws-like { C.new(:s(Str)) }, X::TypeCheck::Assignment,
        "a required :D attribute rejects a type object (construction $_)";
}
for ^2 {
    dies-ok { C.new }, "a missing required attribute dies (construction $_)";
}
for ^2 {
    throws-like { C.new(:s<a>, :n(Int)) }, X::TypeCheck::Assignment,
        "a :D attribute rejects a type object (construction $_)";
}

class MySet is Set { }
for ^2 {
    my $s = MySet.new(<a b a>);
    is $s.elems, 2, "an `is Set` subclass is backed by a Set (construction $_)";
}

class Plain { has $.v }
for ^3 {
    is Plain.new(:v($_)).v, $_, "a class without smileys or roles (construction $_)";
}
