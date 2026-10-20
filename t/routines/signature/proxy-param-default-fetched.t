use Test;

# A readonly parameter whose default evaluates to a Proxy (an `is rw`
# accessor returning one) binds the value the Proxy fetches, as it does for a
# supplied Proxy argument: multi dispatch and type checks see that value.
# mutsu bound the Proxy itself, so `F.decode($encoded)` found no candidate
# for "Any:D" (PDF's `PDF::COS::Stream.decode($encoded = $.encoded)`, #12907).
# Expected values are the `raku` (v2026.09) output.

plan 4;

class F {
    multi method decode(Blob $b) { 'blob ' ~ $b.elems }
    multi method decode(Str $s)  { "str $s" }
}
class S {
    has $!e = Buf.new(1, 2, 3);
    method encoded is rw {
        Proxy.new: FETCH => sub ($) { $!e }, STORE => sub ($, $v) { $!e = $v }
    }
    method decode($encoded = $.encoded) { F.decode($encoded) }
    method typed(Blob $encoded = $.encoded) { $encoded.elems }
    method named(:$encoded = $.encoded) { F.decode($encoded) }
    method what($encoded = $.encoded) { $encoded.VAR.^name }
}

is S.new.decode, 'blob 3', 'a positional default dispatches on the fetched value';
is S.new.typed, 3, 'a typed positional default checks the fetched value';
is S.new.named, 'blob 3', 'a named default dispatches on the fetched value';
is S.new.what, 'Scalar', 'the parameter holds no Proxy';
