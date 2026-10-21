use Test;
use nqp;

plan 9;

# `@!attr := Proxy.new(...)` / `%!attr := Proxy.new(...)` seat the Proxy as the
# attribute (as `$!attr := Proxy` does), after its FETCHed value passes the
# Positional / Associative check; `nqp::bindattr` of a Proxy replaces a slot a
# `get_value` promoted to a cell (#14266, AttrX::Mooish).
my @fetched;
class C {
    has %.h is rw;
    has @.a is rw;
    method bind-hash { %!h := Proxy.new(FETCH => -> $ { @fetched.push: 'h'; {a => 1} }, STORE => -> $, $v { }); self }
    method bind-array { @!a := Proxy.new(FETCH => -> $ { @fetched.push: 'a'; [1, 2] }, STORE => -> $, $v { }); self }
    method peek-h { nqp::getattr(self, C, '%!h').VAR.^name }
    method peek-a { nqp::getattr(self, C, '@!a').VAR.^name }
}

is C.new.bind-hash.peek-h, 'Proxy', 'a bound hash attribute holds the Proxy';
is C.new.bind-array.peek-a, 'Proxy', 'a bound array attribute holds the Proxy';
is C.new.bind-hash.h<a>, 1, 'reading the hash attribute FETCHes';
is-deeply C.new.bind-array.a, [1, 2], 'reading the array attribute FETCHes';

# The FETCHed value is what the bind checks.
my @sink;
throws-like { my @x := Proxy.new(FETCH => -> $ { 5 }, STORE => -> $, $v { }) },
    X::TypeCheck::Binding, 'a Proxy answering a non-Positional does not bind to @';
throws-like { my %x := Proxy.new(FETCH => -> $ { 5 }, STORE => -> $, $v { }) },
    X::TypeCheck::Binding, 'a Proxy answering a non-Associative does not bind to %';
lives-ok { my @x := Proxy.new(FETCH => -> $ { [1] }, STORE => -> $, $v { }) },
    'a Proxy answering an Array binds to @';
lives-ok { my %x := Proxy.new(FETCH => -> $ { {a => 1} }, STORE => -> $, $v { }) },
    'a Proxy answering a Hash binds to %';

# nqp::bindattr replaces a slot that get_value turned into a cell.
class D { has $.v is rw; }
my $d = D.new;
my $attr = D.^attributes[0];
nqp::iscont($attr.get_value($d));
nqp::bindattr($d, D, '$!v', Proxy.new(FETCH => -> $ { 42 }, STORE => -> $, $x { }));
is nqp::getattr($d, D, '$!v').VAR.^name, 'Proxy', 'after get_value, a bound Proxy replaces the slot';
