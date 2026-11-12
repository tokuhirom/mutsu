use Test;

# `.^can` on a class hands back the class's own Method objects, the same ones
# every call, and is memoized per (class, method) for one registry
# generation. Found in Red, whose `Red::Column.gist` asks
# `$model.HOW.^can("as")` for every column it renders.

plan 11;

class MyHOW is Metamodel::ClassHOW { method as(|) { "x" } }
my $t := MyHOW.new_type(:name<T>);
$t.^compose;
is $t.HOW.^can("as").elems, 1, 'a custom HOW method is found on the HOW';
nok $t.HOW.^can("nope"), 'an absent method is not';

class A {
    method foo { 1 }
    multi method bar(Int) { 2 }
    multi method bar(Str) { 3 }
}
class B is A { method foo { 4 } }
is B.^can("foo").elems, 2, 'one entry per declaring class';
is B.new.^can("foo").elems, 2, 'the same from an instance';
ok B.^can("foo")[0] === B.^can("foo")[0], 'repeated calls give the same Method';
is B.^can("bar")[0](B.new, "s"), 3, 'a multi dispatcher entry dispatches';
is A.^can("foo")[0](A.new), 1, 'the parent entry calls the parent method';

A.^add_method("baz", method { 9 });
A.^compose;
is A.^can("baz").elems, 1, 'a method added later is seen';
is B.^can("baz")[0](B.new), 9, 'by a subclass too';

my @hits = (^20).map({ so B.^can("foo") });
ok @hits.all, 'repeated lookups agree';
nok B.^can("missing"), 'a missing method stays missing';
