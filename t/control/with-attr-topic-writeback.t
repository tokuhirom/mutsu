use v6;
use Test;

# `with $!attr { $_ = ... }` writes the topic assignment back to self's
# attribute (raku aliases a bare scalar-variable topic read-write, and an
# attribute variable is such a variable). DBDish::Pg's StatementHandle.finish
# is the load-bearing case:
#
#     method finish() {
#         with $!result { .PQclear; $_ = Nil }
#         ...
#     }
#
# When the `$_ = Nil` did not reach the attribute cell, $!result kept the
# freed C pointer and the next finish PQclear'd it again — a double-free
# SEGV that killed DBIish's t/35-pg-common.rakutest at test 76.

plan 4;

class C {
    has $.r = 42;
    method clear() {
        with $!r { $_ = Nil; }
    }
    method set-inner($v) {
        with $!r { $_ = $v; }
    }
}

my $c = C.new;
$c.set-inner(7);
is $c.r, 7, 'with $!attr topic assignment reaches the attribute';

$c.clear;
nok $c.r.defined, 'with $!attr { $_ = Nil } clears the attribute';

# Calling clear twice must be safe (the finish() double-free shape): the
# second call sees the cleared attribute and its `with` does not fire.
my $fired = 0;
class D {
    has $.h = 1;
    method fin() {
        with $!h { $fired++; $_ = Nil; }
    }
}
my $d = D.new;
$d.fin;
$d.fin;
is $fired, 1, 'second finish sees the cleared attribute (with does not fire)';

# A plain lexical topic still writes back too (regression guard).
my $x = 5;
with $x { $_ = 6; }
is $x, 6, 'with $lexical topic assignment still writes back';
