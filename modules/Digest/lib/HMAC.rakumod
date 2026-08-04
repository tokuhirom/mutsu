unit module HMAC;

proto hmac(
  :$key,
  :$msg,
  :&hash,
  UInt :$block-size
) returns Blob is export {*}


multi hmac(Str :$key,     :$msg, :&hash, :$block-size) { samewith key => $key.encode, :$msg,        :&hash, :$block-size }
multi hmac(    :$key, Str :$msg, :&hash, :$block-size) { samewith       :$key,  msg => $msg.encode, :&hash, :$block-size }

multi hmac(Blob :$key is copy, Blob :$msg, :&hash, :$block-size) {
  if +$key > $block-size { $key .= &hash }
  if +$key < $block-size { $key ~= Blob.new: 0 xx ($block-size - $key) }
  reduce -> $m, $i { &hash(blob8.new(@$key Z[+^] $i xx *) ~ $m) }, $msg, 0x36, 0x5c;
}

# vi: ft=raku
