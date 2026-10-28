use Test;
use nqp;

# `my $its := $!its` shares the attribute's cell; an nqp list op on the
# attribute read back later (here from a subclass method) sees the buffer.
plan 2;

class F {
    has $!its is built;
    method mk(@its) {
        @its.iterator.push-all(my $buf := nqp::create(IterationBuffer));
        my $self := nqp::create(self);
        nqp::bindattr($self, F, '$!its', $buf);
        $self
    }
    method drain { my $its := $!its; my @got; nqp::while(nqp::elems($its), nqp::stmts(@got.push(self.nxt))); @got }
}
class G is F { method nxt { nqp::pop(nqp::getattr(self, F, '$!its')) } }

is G.mk((1, 2, 3)).drain.join(','), '3,2,1', 'nqp::pop on a bound attribute drains it';
is F.mk((4, 5)).WHAT.^name, 'F', 'the constructor still builds the object';
