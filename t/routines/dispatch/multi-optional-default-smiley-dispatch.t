use v6;
use Test;

plan 5;

# An unsupplied optional parameter binds its default (or the bare type object),
# so a `:D` smiley on it must not veto the candidate during multi dispatch --
# raku checks definedness at bind time, against the default value.
# (HTTP::Response's `multi method new(Int:D $code = 200, *%fields)` had to be
# reachable from a bare `.new`.)

class R {
    has $.code is rw;
    submethod BUILD(:$!code) { }
    proto method new(|) {*}
    multi method new(Blob:D $chunk) { self.bless(:code(1)) }
    multi method new(Int:D $code = 200, *%fields) { self.bless(:$code) }
}

is R.new.code, 200, 'the defaulted Int:D candidate matches a no-argument call';
is R.new(404).code, 404, 'an explicit argument still binds';
is R.new(Buf.new(1)).code, 1, 'the Blob:D candidate still wins for a Blob';

# The nominal type keeps discriminating, so an argument of the wrong type does
# not fall through to a defaulted candidate of another type.
proto sub p(|) {*}
multi sub p(Int:D $x = 200) { "int" }
multi sub p(Str:D $s) { "str" }
is p(), "int", 'no argument picks the defaulted candidate';
is p("x"), "str", 'a Str argument picks the Str candidate';
