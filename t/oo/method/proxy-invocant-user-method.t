use Test;

# A method called on a Proxy (an `is rw` accessor's return) dispatches on the
# value the Proxy fetches. A user method of that value's class takes the call
# from the built-in row, as it does without the Proxy: a `Blob`-doing class's
# own `Str` used to lose to `Blob.Str`, which dies. PDF's `PDF::IO::Blob`
# (`$stream.encoded.Str`, #12907). Expected values are the `raku` (v2026.09)
# output.

plan 4;

class B does Blob[uint8] is repr('VMArray') {
    multi method Str { self.decode: 'latin-1' }
    method foo { 'foo' }
}
class S {
    has $!e = B.new(104, 105);
    method enc is rw {
        Proxy.new: FETCH => sub ($) { $!e }, STORE => sub ($, $v) { $!e = $v }
    }
}
sub p($v) is rw { Proxy.new: FETCH => sub ($) { $v }, STORE => sub ($, $) { } }

is S.new.enc.Str, 'hi', "a Proxy-returning accessor's Blob uses the class's own Str";
is p(B.new(104, 105)).Str, 'hi', '... and so does a Proxy-returning sub';
is p(B.new(104, 105)).foo, 'foo', 'a method the built-in type lacks still dispatches';
is p(B.new(104, 105)).elems, 2, 'a built-in method the class does not override still answers';
