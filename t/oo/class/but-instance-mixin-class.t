use Test;

plan 9;

my role R {
    has $.a is rw = 1;
    method hi { "hi" }
}
my class Foo { has $.x = 5 }

my $f = Foo.new(x => 7);
my $p = $f but R;

is $p.^name, "Foo+\{R\}", "a lexical class's instance is mixed into the mixin type";
is $p.WHAT.^name, "Foo+\{R\}", ".WHAT names the mixin type";
is $f.^name, "Foo", "the original object keeps its class";
is $p.x, 7, "attributes of the class are copied";
is $p.a, 1, "the role's attribute is seeded";
is $p.hi, "hi", "the role's method is composed";
ok $p ~~ R, "the mixed object does the role";
nok $f ~~ R, "the original does not";
$p.a = 3;
is (Foo.new but R).a, 1, "mixing into a fresh object starts from the default";
