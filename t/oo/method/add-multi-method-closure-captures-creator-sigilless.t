use v6;
use Test;

plan 5;

# A method added with `^add_multi_method` reads the sigilless parameter of the
# routine that built it (`\type`). A closure created in that method's body has
# to capture it: it runs after the method returned, when no frame has `type`.
class Maker {
    method add-all(Mu:U \type) {
        type.^add_multi_method: "returned", method (::T Mu:U:) {
            -> $ { "returned " ~ type.^name }
        }
        type.^add_multi_method: "proxied", method (::T Mu:U:) is rw {
            Proxy.new:
                FETCH => -> $ { "fetched " ~ type.^name },
                STORE => -> $, $v { }
        }
        type.^add_multi_method: "inline", method (::T Mu:U:) {
            my $f = -> $ { "inline " ~ type.^name };
            $f(1)
        }
    }
}
class Alpha { }
class Beta { }
Maker.new.add-all(Alpha);
Maker.new.add-all(Beta);
Alpha.^compose;
Beta.^compose;

my $f = Alpha.returned;
sub elsewhere(\type) { $f(1) }
is $f(1), 'returned Alpha', 'a returned closure keeps the creator parameter';
is elsewhere(Str), 'returned Alpha', 'and is not fooled by a same-named caller binding';
is Beta.returned()(1), 'returned Beta', 'each type gets its own capture';
is Alpha.proxied, 'fetched Alpha', 'a Proxy FETCH closure keeps it';
is Alpha.inline, 'inline Alpha', 'a closure run inside the method still sees it';
