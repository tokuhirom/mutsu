use Test;
use nqp;

plan 4;

# A method that assigns through its raw invocant (`nqp::assign(SELF, ...)`)
# writes into the caller's variable, also when the only use of that variable
# is a method call inside a closure running on another thread.
class Tr {
    method hit(Tr:D \SELF:) is raw {
        nqp::assign(SELF, 42) if nqp::iscont(SELF);
        1
    }
}

my $x = Tr.new;
$x.hit;
is $x, 42, 'plain call assigns through SELF';

my $y = Tr.new;
await start { $y.hit };
is $y, 42, 'a start block that only calls the method assigns the outer variable';

my $z = Tr.new;
my $p = start { $z.hit };
await $p;
is $z, 42, 'same through a stored promise';

my $w = Tr.new;
my &f = -> { $w.hit };
f();
is $w, 42, 'same through a closure called later';
