use Test;

# `has $.x` generates a read-only accessor, but an explicit `method x is rw`
# replaces it: `$.x = v` stores through that method (a Proxy here), not to the
# attribute (#12907, PDF::COS::Stream.edit-stream).
plan 4;

class B does Blob[uint8] is repr('VMArray') {
    multi method Stringy { self.decode: "latin-1" }
}

class S {
    has $.decoded;
    method decoded is rw {
        sub FETCH($) { $!decoded }
        sub STORE($, $v) { $!decoded = $v }
        Proxy.new: :&FETCH, :&STORE;
    }
    method seed($v) { $!decoded = $v }
    method edit(Str :$prepend = '', Str :$append = '') {
        $.decoded = $prepend ~ ($!decoded // '') ~ $append;
    }
}

my $s = S.new;
$s.seed("abc");
$s.edit(:prepend('q '), :append(' Q'));
is $s.decoded, 'q abc Q', 'a Str attribute behind the rw method is rewritten';

my $b = S.new;
$b.seed(B.new("abc".encode("latin-1")));
$b.edit(:prepend('q '), :append(' Q'));
is $b.decoded, 'q abc Q', 'so is a Blob (immutable value) one';

class R { has $.x; method bump { $.x = 5 } }
throws-like { R.new(:x(1)).bump }, X::Assignment::RO, 'a plain read-only accessor still refuses assignment';

class P {
    has $.y;
    method y { $!y }
    method set { $.y = 5 }
}
throws-like { P.new(:y(1)).set }, X::Assignment::RO, 'a non-rw method replacing the accessor refuses too';
