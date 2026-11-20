use Test;

# `@!x` / `%!x` read, `push` and method calls go through an attribute slot
# that refers to the instance's own container (ADR-18356 D2, #18356). Each
# case is checked against rakudo.

plan 21;

# Push and method calls reach the instance, across calls and instances.
{
    my class Lines {
        has @!lines;
        has %!seen;
        method add($l) { @!lines.push($l); %!seen{$l} = True; self }
        method count { @!lines.elems }
        method last { @!lines.tail }
        method drop { @!lines.pop }
        method keys { %!seen.keys.sort.join(',') }
        method all { @!lines.join('|') }
    }
    my $a = Lines.new;
    my $b = Lines.new;
    $a.add('x').add('y');
    $b.add('z');
    is $a.count, 2, 'pushes accumulate on the instance';
    is $b.count, 1, 'and each instance has its own array';
    is $a.last, 'y', 'a method call reads the pushed element';
    is $a.drop, 'y', 'pop removes from the instance';
    is $a.all, 'x', 'and the instance keeps the result';
    is $a.keys, 'x,y', 'hash attribute stores reach the instance';
}

# A push inside a loop, a closure and a nested method call.
{
    my class Acc {
        has @!a;
        method fill($n) { @!a.push($_) for ^$n; self }
        method via-closure { my &f = { @!a.push('c') }; f(); f(); @!a.elems }
        method inner { @!a.push('i') }
        method outer { @!a.push('o'); self.inner; @!a.push('o2'); @!a.join(',') }
    }
    my $o = Acc.new;
    is $o.fill(3).via-closure, 5, 'loop and closure pushes all land';
    is Acc.new.outer, 'o,i,o2', 'a nested method push between two pushes is kept';
}

# Typed, bound and immutable aggregates.
{
    my class T {
        has Int @!ints;
        has @!bound;
        has @!list;
        method push-int($v) { @!ints.push($v); @!ints.elems }
        method bind(@src) { @!bound := @src }
        method push-bound($v) { @!bound.push($v) }
        method bind-list { @!list := (1, 2) }
        method push-list { @!list.push(3) }
    }
    my $t = T.new;
    is $t.push-int(4), 1, 'a typed array attribute accepts a matching push';
    throws-like { $t.push-int('x') }, X::TypeCheck, 'and rejects a mismatched one';
    my @src = 1;
    $t.bind(@src);
    $t.push-bound(2);
    is @src.join(','), '1,2', 'a push to a bound array attribute reaches the source';
    $t.bind-list;
    dies-ok { $t.push-list }, 'a push to a List-bound attribute dies';
}

# Roles, inheritance and a runtime mixin.
{
    my role R { has @!r; method r-add($v) { @!r.push($v); @!r.elems } }
    my class P { has @!p; method p-add($v) { @!p.push($v); @!p.elems } }
    my class C is P does R { has @!c; method c-add($v) { @!c.push($v); @!c.elems } }
    my $c = C.new;
    $c.r-add(1); $c.p-add(1); $c.c-add(1);
    is "{$c.r-add(2)} {$c.p-add(2)} {$c.c-add(2)}", '2 2 2', 'role, parent and own array attributes';
    my role M { has @!m; method m-add($v) { @!m.push($v); @!m.elems } }
    my $x = P.new but M;
    $x.m-add('a');
    is $x.m-add('b'), 2, 'a runtime mixin role array attribute';
}

# BUILD and TWEAK see and fill the attribute.
{
    my class B {
        has @!a;
        submethod BUILD(:@init) { @!a.push($_) for @init }
        submethod TWEAK { @!a.push('tweak') }
        method all { @!a.join(',') }
    }
    is B.new(init => <p q>).all, 'p,q,tweak', 'BUILD and TWEAK push into the attribute';
}

# A whole store between pushes replaces the container the next push sees.
{
    my class W {
        has @!a;
        method go { @!a.push(1); @!a = <x y>; @!a.push(2); @!a.join(',') }
        method reset { @!a = () }
        method n { @!a.elems }
    }
    my $w = W.new;
    is $w.go, 'x,y,2', 'push after a whole assignment appends to the new contents';
    $w.reset;
    is $w.n, 0, 'a whole assignment from another method is seen';
}

# The array a method hands out is the attribute itself.
{
    my class H { has @!a; method a { @!a }; method n { @!a.elems } }
    my $h = H.new;
    $h.a.push(5);
    is $h.n, 1, 'a push through the returned array reaches the attribute';
}

# A public aggregate attribute through its private name.
{
    my class Q { has @.items; method add($v) { @!items.push($v) } }
    my $q = Q.new(items => [1]);
    $q.add(2);
    is $q.items.join(','), '1,2', 'the accessor sees a private-name push';
}

# Pushes from several threads to one attribute, serialized by a lock.
{
    my class S { has @!a; has $!l = Lock.new; method add($v) { $!l.protect: { @!a.push($v) } }; method n { @!a.elems } }
    my $s = S.new;
    await (^4).map: -> $t { start { $s.add($_) for ^50 } };
    is $s.n, 200, 'locked pushes from four threads all land';
}

# An attribute read in a method that never pushes still reads the instance.
{
    my class E { has %!h; method set($k) { %!h{$k} = 1 }; method has($k) { %!h{$k}:exists } }
    my $e = E.new;
    $e.set('a');
    ok $e.has('a'), 'a hash attribute element written elsewhere is read';
}
