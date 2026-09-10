use v6;
use Test;

plan 5;

# An `is rw` routine returns its LAST expression's container, so a body that
# does some work before exposing `$!attr` is still an assignable lvalue.
# (HTTP::Request lazily defaults its scheme this way:
#  `multi method scheme(--> Str:D) is rw { without $!scheme { … }; $!scheme }`)

class Lazy {
    has Str $!scheme;
    has Int $.defaulted = 0;
    method scheme(--> Str:D) is rw {
        without $!scheme {
            $!defaulted++;
            $!scheme = 'http';
        }
        $!scheme
    }
}

my $l = Lazy.new;
is $l.scheme, 'http', 'the lazy default is applied on read';
is $l.defaulted, 1, 'and the leading statements ran';

$l.scheme = 'https';
is $l.scheme, 'https', 'assigning through the rw method stores into the attribute';

# The single-statement form keeps working.
class Simple {
    has $!v;
    method v is rw { $!v }
}
my $s = Simple.new;
$s.v = 42;
is $s.v, 42, 'a one-statement rw method is still an lvalue';

# A body that does NOT end in an attribute is still not assignable.
class NoAttr {
    has $!v;
    method computed is rw { $!v; 1 }
}
dies-ok { NoAttr.new.computed = 5 },
    'an rw method whose body ends in a non-attribute is not assignable';
