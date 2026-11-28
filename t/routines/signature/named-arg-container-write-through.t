use Test;

# A `:$var` argument of a method call hands the callee the variable's
# container: an `is rw`/raw binding, a capture and a slurpy hash write through
# to it, while the default constructor only copies its value (#19384).

plan 17;

# The default constructor copies the value.
class C { has $.x; has @.a; has Int $.n }
{
    my $x = 5; my $arr = [1, 2, 3]; my Int $n = 3;
    my $c = C.new(:$x, :a($arr), :$n);
    is $c.x, 5, 'scalar value';
    is $c.a.elems, 1, 'a Scalar holding an Array is one item';
    is $c.n, 3, 'typed variable';
    my $lst = (4, 5);
    is C.new(:a($lst)).a.elems, 1, 'a Scalar holding a List is one item';
    my $y = 1;
    is C.new(:x($y), :n($y = 7)).x, 7, 'a later argument assigning the variable is seen';
    my $k := $x;
    is C.new(:x($k)).x, 5, 'a bound alias';
    sub inner() { my $q = 3; my $r = C.new(:x($q)); $q = 4; $r.x }
    is inner(), 3, 'the attribute keeps the value, not the variable';
}

# A constructor that runs user code over the arguments still gets the container.
{
    my class D { has $.x; method new(:$x! is rw) { $x = 99; self.bless(:$x) } }
    my $y = 1; D.new(:x($y));
    is $y, 99, 'user new binding is rw';
    my class E { has $.x; submethod BUILD(:$x! is rw) { $x = 42 } }
    my $z = 1; E.new(:x($z));
    is $z, 42, 'BUILD binding is rw';
}

# Any other method gets the container.
{
    my class F {
        method rw(:$x! is rw) { $x = 7 }
        method raw(:$x! is raw) { $x = 8 }
        method slurpy(*%h) { %h<x> = 9 }
    }
    my $w = 1; F.rw(:x($w));     is $w, 7, 'is rw';
    $w = 1;    F.new.rw(:x($w)); is $w, 7, 'is rw on an instance';
    $w = 1;    F.raw(:x($w));    is $w, 8, 'is raw';
    $w = 1;    F.slurpy(:x($w)); is $w, 9, 'slurpy hash';
    my $o = F.new; my $v;
    $v = 1; $o.rw(:x($v)); is $v, 7, 'method on a variable receiver';
}

# The type constraint travels with the container.
{
    my class G { method m(:$x! is rw) { $x = "s" } }
    my Int $n = 1;
    throws-like { G.m(:x($n)) }, X::TypeCheck::Assignment, 'a typed variable refuses a Str';
    is $n, 1, 'and keeps its value';
    my class H { has Str $.s }
    my Int $i = 3;
    throws-like { H.new(:s($i)) }, X::TypeCheck::Assignment, 'attribute type check';
}
