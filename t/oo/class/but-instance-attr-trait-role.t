use Test;

plan 6;

my @seen;
multi sub trait_mod:<is>(Attribute:D $attr, :$tagged!) {
    @seen.push($attr.name);
}

my role R {
    has $.v is tagged is rw = "init";
    method hi { "hi" }
}
my class Foo { has $.x = 5 }

my $f = Foo.new(x => 7);
my $p = $f but R;

is $p.^name, "Foo+\{R\}", "the instance is mixed into the mixin type";
is $p.x, 7, "the class's attribute is copied";
is $p.v, "init", "the role's attribute holds its default";
is $p.hi, "hi", "the role's method is composed";
nok $f ~~ R, "the original object is untouched";
ok @seen.elems >= 1, "the attribute trait ran";
