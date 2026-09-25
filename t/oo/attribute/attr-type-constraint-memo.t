use Test;

# An attribute assignment inside a method (`$!x = v`) checks the attribute's
# declared type, which is looked up per `(class, attribute)` and memoized
# until the next registry write (ADR-0121). These pin that the memo answers
# per class and follows changes made after it was filled.

plan 10;

class Typed {
    has Int $.n = 0;
    has $.free;
    method set-n($v) { $!n = $v }
    method set-free($v) { $!free = $v }
}

{
    my $t = Typed.new;
    $t.set-n($_) for ^5;
    is $t.n, 4, 'repeated typed stores';
    throws-like { $t.set-n('str') }, X::TypeCheck::Assignment,
        'a typed store still rejects a wrong type after many hits';
    $t.set-free($_) for 'a', 1, 2.5;
    is $t.free, 2.5, 'an untyped attribute accepts anything';
}

# One method body, two classes: the parent's attribute is typed for the
# parent, and a subclass instance runs the same store.
{
    class Kid is Typed { has Str $.extra = 'e' }
    my @objs = Typed.new, Kid.new, Typed.new;
    .set-n(7) for @objs;
    is @objs.map(*.n).join(','), '7,7,7', 'the inherited typed store on two classes';
    throws-like { Kid.new.set-n('x') }, X::TypeCheck::Assignment,
        'the subclass inherits the check';
}

# The same attribute name, typed differently in two unrelated classes, stored
# from the same-named method: the memo is per class.
{
    class A1 { has Int $.v; method put($x) { $!v = $x } }
    class B1 { has Str $.v; method put($x) { $!v = $x } }
    my $a = A1.new; my $b = B1.new;
    for ^3 { $a.put(1); $b.put('s') }
    is "{$a.v}{$b.v}", '1s', 'each class checks its own declaration';
    throws-like { $a.put('s') }, X::TypeCheck::Assignment, 'Int attribute rejects Str';
    throws-like { $b.put(1) }, X::TypeCheck::Assignment, 'Str attribute rejects Int';
}

# Assigning Nil resets a typed attribute to its own type object.
{
    class N1 { has Int $.v = 3; method clear { $!v = Nil } }
    my $o = N1.new;
    $o.clear for ^2;
    ok $o.v === Int, 'Nil resets to the declared type object';
}

# A class declared after the memo was filled does not leave a stale answer:
# a subclass declared at run time through EVAL sees its own attribute type.
{
    class Late { has $.x; method put($v) { $!x = $v } }
    my $l = Late.new;
    $l.put($_) for ^3;
    my $sub = EVAL 'class LateKid is Late { has Int $.y; method puty($v) { $!y = $v } }; LateKid';
    my $k = $sub.new;
    $k.puty(1);
    throws-like { $k.puty('no') }, X::TypeCheck::Assignment,
        'a class declared later is checked against its own declaration';
}
