use Test;

plan 1;

sub next-byte($byte) {
    given $byte {
        when * < 32 { $byte }
        when * +& 0xe0 == 0xa0 { decode-string(length => $byte +& 0x1f) }
        when $byte == 0xc3 { True }
        default { $byte }
    }
}

sub decode-string(:$length) {
    my $remaining = $length;
    my $text = '';
    sub ($byte) {
        $text ~= $byte.chr;
        return &?BLOCK if --$remaining;
        $text;
    }
}

sub decode-map(:$length) {
    my $remaining = $length;
    my $value-decoder = &next-byte;
    my @pairs;
    sub ($byte) {
        $value-decoder = $value-decoder.($byte);
        if $value-decoder !~~ Sub {
            @pairs.push($value-decoder);
            $remaining-- if @pairs.elems %% 2;
            return @pairs.Hash unless $remaining;
            $value-decoder = &next-byte;
        }
        return &?BLOCK;
    }
}

my $decoder = decode-map(length => 3);
my @decoded;
for 0xa2, 0x69, 0x64, 1,
    0xa4, 0x74, 0x79, 0x70, 0x65, 4,
    0xa9, 0x73, 0x75, 0x73, 0x70, 0x65, 0x6e, 0x64, 0x65, 0x64, 0xc3
    -> $byte
{
    $decoder = $decoder.($byte);
    @decoded.push($decoder) unless $decoder ~~ Callable;
}

is-deeply @decoded[0], { id => 1, type => 4, suspended => True },
    'a closure parser keeps its continuation across split input chunks';
