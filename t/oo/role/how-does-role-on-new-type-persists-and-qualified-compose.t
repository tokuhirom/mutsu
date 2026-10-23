use v6;
use Test;

plan 7;

my $composes = 0;
class MyHOW is Metamodel::ClassHOW {
    method compose(Mu \type) {
        $composes++;
        self.Metamodel::ClassHOW::compose(type);
    }
    method foo(|) { "base-foo" }
}

role R[\a] {
    method foo(|) { "role-foo" }
    method columns(|) { <x y> }
    method table(|) { "tbl" }
}

my \plain = MyHOW.new.new_type(:name<HowDoesPlain>);
plain.^compose;
is plain.^foo, "base-foo", "a type without a mixin uses the HOW class's method";
is $composes, 1, "the user compose ran once and reached the native one";

my \alias = MyHOW.new.new_type(:name<HowDoesAlias>);
alias.HOW does R[alias];
alias.^compose;
is $composes, 2, "a composed HOW with a mixin still runs the user compose once, not recursively";
is alias.^foo, "role-foo", "HOW does R persists on the new type's HOW";
is alias.^columns.join(","), "x,y", "the mixed-in methods answer through ^";
is alias.^table, "tbl", "all of them";
ok alias.HOW.^name.starts-with("MyHOW+"), "the HOW is the mixin";
