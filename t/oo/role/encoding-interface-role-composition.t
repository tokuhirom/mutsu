use Test;

plan 4;

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

# The registry's encoder is Encoding::Encoder::Builtin doing the role (#11783).
my $enc = Encoding::Registry.find("utf8").encoder;
is $enc.^name, 'Encoding::Encoder::Builtin', 'builtin encoder class name';
ok $enc.does(Encoding::Encoder), 'builtin encoder does Encoding::Encoder';
