use Test;

plan 2;

# Encoding::Decoder and Encoding::Encoder are composable interfaces in Raku,
# even though mutsu also has native dispatch entries for their methods.
class DecoderProbe does Encoding::Decoder {
    method add-bytes(Blob:D $bytes --> Nil) { }
    method consume-available-chars(--> Str:D) { '' }
    method consume-all-chars(--> Str:D) { '' }
    method consume-exactly-chars(Int:D $chars, Bool :$eof = False --> Str) { '' }
    method consume-line-chars(Bool :$chomp = False, Bool :$eof = False --> Str) { '' }
    method consume-exactly-bytes(Int:D $bytes --> Blob) { Blob }
    method bytes-available(--> Int:D) { 0 }
    method is-empty(--> Bool:D) { True }
    method set-line-separators(@seps --> Nil) { }
}

class EncoderProbe does Encoding::Encoder {
    method encode-chars(Str:D $str --> Blob:D) { Blob }
}

ok DecoderProbe.^roles.grep(*.^name eq 'Encoding::Decoder'),
    'Encoding::Decoder is composable';
ok EncoderProbe.^roles.grep(*.^name eq 'Encoding::Encoder'),
    'Encoding::Encoder is composable';
