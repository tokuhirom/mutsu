use Test;

plan 8;

# Calling an `is rw` sub whose result is a `Proxy` must FETCH it in value
# context. Every branch of `dispatch_func_call_inner` did that except the four
# that run the callee as on-the-fly-compiled bytecode, which is the branch a
# file-scope sub takes when it is called from a *method* body (the method's
# compiled-function table does not carry it). The Proxy was then stored raw, so
# the FETCH happened later — at the next read — and escaped any `try` around the
# call.
#
# This is what made `NativeLibs::Loader`'s
# `(try cglobal($lib, $sym, Pointer)) ~~ Pointer` probe throw instead of
# answering False for a library that is not installed.

my $fetches = 0;
sub mk($ok) is rw {
    Proxy.new(FETCH => -> $ { $fetches++; $ok ?? 42 !! die "boom" }, STORE => -> $, $ { })
}

sub from-sub()    { my $x = mk(True); $x }
class C {
    method from-method()      { my $x = mk(True); $x }
    method !from-private()    { my $x = mk(True); $x }
    method via-private()      { self!from-private }
    method in-loop()          { my @r; for 1, 2 { @r.push: (my $x = mk(True)) }; @r }
}

$fetches = 0;
is from-sub(), 42, 'an is-rw sub call in a sub FETCHes the Proxy';
is $fetches, 1, 'exactly one FETCH from the sub call';

$fetches = 0;
is C.from-method, 42, 'the same call from a method body FETCHes it too';
is $fetches, 1, 'exactly one FETCH from the method call';

$fetches = 0;
is C.via-private, 42, 'and from a private method';
is C.in-loop.join(','), '42,42', 'and once per iteration inside a loop';

# A throwing FETCH must be caught by a `try` around the call, in a method just
# as in a sub — it used to escape because the FETCH was deferred past the `try`.
sub caught-in-sub()    { (try mk(False)).defined }
class D { method caught-in-method() { (try mk(False)).defined } }

nok caught-in-sub(),        'a throwing FETCH is caught by try in a sub';
nok D.caught-in-method,     'a throwing FETCH is caught by try in a method';
