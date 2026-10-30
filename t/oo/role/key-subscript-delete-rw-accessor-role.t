use Test;

plan 6;

# `X<k>:delete` calls a user DELETE-KEY of the object X, also when X is the
# result of a method (here an `is rw` accessor, which answers a Proxy), a sub
# or the topic of a block. Red's `.num1<ble>:delete` is this shape; the delete
# was dropped silently and Red took the map for a projection.

my @log;
role Meths {
    multi method DELETE-KEY(\SELF: $key where { $_ ~~ Str }) { @log.push: "DELETE-KEY $key"; "deleted-$key" }
}
class Col does Meths { has $.returns = Int }
class Model {
    has $.col = Col.new;
    method num1 is rw { Proxy.new(FETCH => -> $ { $!col }, STORE => -> $, $v { $!col = $v }) }
}
class Plain { method num1 { Col.new } }

my $m = Model.new;
is $m.num1<ble>:delete, "deleted-ble", 'an rw accessor (a Proxy) as the receiver';
is @log.tail, "DELETE-KEY ble", '... called the object\'s DELETE-KEY';
is Plain.num1<t>:delete, "deleted-t", 'a method result as the receiver';
sub mk { Col.new }
is mk()<y>:delete, "deleted-y", 'a sub result as the receiver';
for Plain { is .num1<q>:delete, "deleted-q", 'the topic\'s accessor' }
my $c = Col.new;
is $c<x>:delete, "deleted-x", 'a variable receiver';
