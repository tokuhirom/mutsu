use Test;

# URI::Encode uses a named sub as a .subst replacement to turn percent-encoded
# bytes back into text. A named sub is a Routine value, not a closure Sub.

plan 2;

sub decode-percent($m) {
    Buf.new($m<bit>.list.map({ :16($_.Str) })).decode
}

is '%20%20'.subst(/[\%$<bit>=[<[0..9A..Fa..f]>** 2]]+/, &decode-percent, :g),
    '  ', 'a named sub replacement receives the Match';
is '%C3%A5'.subst(/[\%$<bit>=[<[0..9A..Fa..f]>** 2]]+/, &decode-percent, :g),
    'å', 'a named sub replacement can decode UTF-8 bytes';
