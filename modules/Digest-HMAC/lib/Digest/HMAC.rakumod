unit module Digest::HMAC;

our sub hmac($key is copy, $message is copy, &hash, $blocksize = 64) is export {
    $key = $key.encode('ascii') unless $key ~~ Blob;
    $message = $message.encode('ascii') unless $message ~~ Blob;

    if $key.list.elems > $blocksize {
        $key = hash($key);
    }
    if $key.list.elems < $blocksize {
        $key ~= Blob[uint8].new(0x00 xx ($blocksize - $key.list.elems));
    }

    # $i_key_pad ~ $message
    my $inner-buf = hash((Blob.new(0x36 xx $blocksize) ~^ $key) ~ $message);

    # $o_key_pad ~ $inner
    return hash((Blob.new(0x5c xx $blocksize) ~^ $key) ~ $inner-buf);
}

our sub hmac-hex($key, $message, &hash, $blocksize = 64) is export {
    return hmac($key, $message, &hash, $blocksize).list».fmt("%02x").join;
}
