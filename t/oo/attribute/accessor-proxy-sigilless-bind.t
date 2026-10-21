use Test;

# `$.x` is whatever container `self.x` returns. When a user `x` method
# returns a `Proxy`, binding a sigilless name to `$.x` binds that Proxy; it
# must not alias the attribute `$!x`. mutsu stored the Proxy into `$!x`, so
# the Proxy's FETCH, which reads `$!x`, recursed forever (PDF's
# `PDF::COS::Stream.content`: `my \encoded = $.encoded`). Expected values are
# the `raku` (v2026.09) output.

plan 6;

class S {
    has $.encoded;
    has $.decoded;
    method encoded is rw {
        Proxy.new:
            FETCH => sub ($) { $!encoded //= "E:$_" with $!decoded; $!encoded },
            STORE => sub ($, $s) { $!encoded = $s };
    }
    method content {
        my \encoded = $.encoded;
        with encoded { 'got:' ~ .Str } else { 'none' }
    }
    method twice { my \e = $.encoded; e ~ '/' ~ e }
    method raw-attr { my \e = $.encoded; e; $!encoded.VAR.^name }
}

is S.new(:decoded<abc>).content, 'got:E:abc', 'a sigilless bind of $.x reads through the Proxy';
is S.new(:encoded<xyz>).content, 'got:xyz', '... with the attribute already set';
is S.new.content, 'none', '... and an undefined result';
is S.new(:encoded<q>).twice, 'q/q', 'the binding can be read more than once';
is S.new(:encoded<q>).raw-attr, 'Scalar', 'the attribute keeps its own container';

class R {
    has $.v is rw = 1;
    method a { my \e = $.v; e = 5; $!v }
}
is R.new.a, 5, 'an is-rw attribute accessor still aliases the attribute';
