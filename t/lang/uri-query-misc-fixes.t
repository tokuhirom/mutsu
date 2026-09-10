use v6;
use Test;

plan 6;

# `flat` splices a bare hash's pairs (URI::Query's `flat %new, @new`).
my %empty;
is-deeply [flat %empty, (foo => 1)], [foo => 1],
    'flat drops an empty hash entirely';
my %one = a => 1;
is-deeply [flat %one, (foo => 1)], [a => 1, foo => 1],
    'flat splices hash pairs';
my $itemized = %(x => 1);
is flat(0, $itemized).elems, 2, 'an itemized hash stays a single element';

# An attribute smartmatch substitution writes back to the attribute cell
# (URI::Query's `$!query ~~ s/'=' $//`).
class C {
    has $.s = "foo=";
    method fix() { $!s ~~ s/'=' $// unless $!s ~~ /'&'/; $!s }
}
is C.new.fix, 'foo', '$!attr ~~ s/// persists in the attribute';

# `return-rw $!attr` exposes a writable lvalue without an `is rw` trait
# (URI's `multi method fragment { return-rw $!fragment }`).
class R {
    has $.f = '';
    multi method g(R:D:) { return-rw $!f }
    multi method g(R:D: Str() $n) { $!f = $n }
}
my $r = R.new;
$r.g = 'x';
is $r.f, 'x', 'assignment through a return-rw multi method works';
$r.g('y');
is $r.f, 'y', 'the setter multi candidate still dispatches';
