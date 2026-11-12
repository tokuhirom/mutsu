use Test;

# `--> Mu` admits every value: a type object, Nil, a Failure, a junction, and
# a container or a Proxy from an `is rw` routine, which is handed back as it is.
# (Monad's type hooks are all `method maybe-type(--> Mu)`, #17331.)

plan 8;

sub type-obj(--> Mu) { Int }
sub nil(--> Mu) { Nil }
sub junc(--> Mu) { 1 | 2 }
sub fail-it(--> Mu) { fail 'nope' }
my $store = 1;
sub rw(--> Mu) is rw { $store }
my $fetched = 0;
sub proxy(--> Mu) is rw {
    Proxy.new(FETCH => method () { $fetched++; 7 }, STORE => method ($v) { $store = $v })
}

is type-obj().^name, 'Int', 'a type object';
ok nil() === Nil, 'Nil';
isa-ok junc(), Junction, 'a Junction';
ok fail-it() ~~ Failure, 'a Failure';
rw() = 5;
is $store, 5, 'an is rw routine still returns its container';
proxy() = 9;
is $store, 9, 'a Proxy is returned as it is';
is proxy(), 7, 'and reads through FETCH';

class M { method hook(--> Mu) { M } }
is M.new.hook.^name, 'M', 'a method returning its type object';
