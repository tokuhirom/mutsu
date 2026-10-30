use Test;

plan 5;

# A `multi sub` declared in a method closes over the method's `self`, also
# when its `&name` outlives the call (CSS::Properties' Proxy STORE does this).
class A {
    has $.n = 7;
    has %!v;
    method !c($x) { $x }
    method priv { multi sub S($x) { self!c($x) }; &S }
    method attr { multi sub S($x) { %!v<a> = $x; self.n }; &S }
    method pub { multi sub S($, $x) { self.n + $x }; &S }
    method two {
        multi sub S($x where * > 0) { self.n + $x }
        multi sub S($x) { self.n - $x }
        &S
    }
    method v { %!v<a> }
}
my $a = A.new;
my &priv = $a.priv;
is priv(3), 3, 'a private-method call through the returned multi';
my &attr = $a.attr;
is attr(4), 7, 'self.n and an attribute store through the returned multi';
is $a.v, 4, 'the attribute was stored on the creator';
my &pub = $a.pub;
is pub(0, 1), 8, 'an anonymous first parameter';
my &two = $a.two;
is two(1) + two(-1), 8 + 8, 'two candidates both see self';
