use Test;
use nqp;

plan 9;

# A method that aliases an attribute (`my $p := $!cur`) or calls a mutating
# method on it must not, at its exit, undo a `:=` rebind a nested call made.
class C {
    has $!cur = 1;
    method set() { $!cur := 2; self }
    method get() { $!cur }
    method a() { self.set; 0 }
    method b() { my $p := $!cur; self.set; 0 }
    method c() { my $p = $!cur; self.set; 0 }
    method in-frame() { my $p := $!cur; self.set; "$!cur/$p" }
}
for <a b c> -> $m {
    my $x = C.new;
    $x."$m"();
    is $x.get, 2, "$m: the nested rebind survives the outer method's exit";
}
is C.new.in-frame, '2/1', 'inside the frame the attribute is the new binding, the alias the old one';

class D {
    has $!v = 1;
    method write-through { my $p := $!v; $p = 5; $!v }
    method read-through { my $p := $!v; $!v = 7; $p }
    method inc { my $p := $!v; $p++; $p++; $!v }
}
is D.new.write-through, 5, 'writing the alias writes the attribute';
is D.new.read-through, 7, 'the alias sees a write to the attribute';
is D.new.inc, 3, 'an increment through the alias reaches the attribute';

# The shape of ForwardIterables.pull-one: a mutating call on the attribute,
# then a recursive pull after next-iterator rebound it.
class FI {
    has $!iterables;
    has $!current;
    method next-iterator() {
        my $next := nqp::shift($!iterables);
        $!current := $next.iterator;
        self
    }
    method init(@l) {
        $!iterables := nqp::create(IterationBuffer);
        @l.iterator.push-all($!iterables);
        self.next-iterator
    }
    method pull-one() is raw {
        my $pulled := $!current.pull-one;
        nqp::eqaddr($pulled, IterationEnd) && nqp::elems($!iterables)
          ?? (return-rw self.next-iterator.pull-one)
          !! $pulled
    }
}
my $fi = FI.new.init(((1, 2), (3, 4)));
my @got;
loop { my $v := $fi.pull-one; last if nqp::eqaddr($v, IterationEnd); @got.push($v) }
is-deeply @got, [1, 2, 3, 4], 'a rebind made by a nested call outlives the pulling frame';
my $fi2 = FI.new.init(((1,), (2,)));
is $fi2.pull-one, 1, 'first element';
