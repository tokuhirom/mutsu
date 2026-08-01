# Static HPACK Huffman codes table, per RFC 7541 Appendix B.
my constant HUFFMAN_CODES = array[int].new(
    0b1111111111000, 0b11111111111111111011000, 0b1111111111111111111111100010,
    0b1111111111111111111111100011, 0b1111111111111111111111100100,
    0b1111111111111111111111100101, 0b1111111111111111111111100110,
    0b1111111111111111111111100111, 0b1111111111111111111111101000,
    0b111111111111111111101010, 0b111111111111111111111111111100,
    0b1111111111111111111111101001, 0b1111111111111111111111101010,
    0b111111111111111111111111111101, 0b1111111111111111111111101011,
    0b1111111111111111111111101100, 0b1111111111111111111111101101,
    0b1111111111111111111111101110, 0b1111111111111111111111101111,
    0b1111111111111111111111110000, 0b1111111111111111111111110001,
    0b1111111111111111111111110010, 0b111111111111111111111111111110,
    0b1111111111111111111111110011, 0b1111111111111111111111110100,
    0b1111111111111111111111110101, 0b1111111111111111111111110110,
    0b1111111111111111111111110111, 0b1111111111111111111111111000,
    0b1111111111111111111111111001, 0b1111111111111111111111111010,
    0b1111111111111111111111111011, 0b010100, 0b1111111000, 0b1111111001,
    0b111111111010, 0b1111111111001, 0b010101, 0b11111000, 0b11111111010,
    0b1111111010, 0b1111111011, 0b11111001, 0b11111111011, 0b11111010, 0b010110,
    0b010111, 0b011000, 0b00000, 0b00001, 0b00010, 0b011001, 0b011010, 0b011011,
    0b011100, 0b011101, 0b011110, 0b011111, 0b1011100, 0b11111011,
    0b111111111111100, 0b100000, 0b111111111011, 0b1111111100, 0b1111111111010,
    0b100001, 0b1011101, 0b1011110, 0b1011111, 0b1100000, 0b1100001, 0b1100010,
    0b1100011, 0b1100100, 0b1100101, 0b1100110, 0b1100111, 0b1101000, 0b1101001,
    0b1101010, 0b1101011, 0b1101100, 0b1101101, 0b1101110, 0b1101111, 0b1110000,
    0b1110001, 0b1110010, 0b11111100, 0b1110011, 0b11111101, 0b1111111111011,
    0b1111111111111110000, 0b1111111111100, 0b11111111111100, 0b100010,
    0b111111111111101, 0b00011, 0b100011, 0b00100, 0b100100, 0b00101, 0b100101,
    0b100110, 0b100111, 0b00110, 0b1110100, 0b1110101, 0b101000, 0b101001,
    0b101010, 0b00111, 0b101011, 0b1110110, 0b101100, 0b01000, 0b01001,
    0b101101, 0b1110111, 0b1111000, 0b1111001, 0b1111010, 0b1111011,
    0b111111111111110, 0b11111111100, 0b11111111111101, 0b1111111111101,
    0b1111111111111111111111111100, 0b11111111111111100110,
    0b1111111111111111010010, 0b11111111111111100111, 0b11111111111111101000,
    0b1111111111111111010011, 0b1111111111111111010100,
    0b1111111111111111010101, 0b11111111111111111011001,
    0b1111111111111111010110, 0b11111111111111111011010,
    0b11111111111111111011011, 0b11111111111111111011100,
    0b11111111111111111011101, 0b11111111111111111011110,
    0b111111111111111111101011, 0b11111111111111111011111,
    0b111111111111111111101100, 0b111111111111111111101101,
    0b1111111111111111010111, 0b11111111111111111100000,
    0b111111111111111111101110, 0b11111111111111111100001,
    0b11111111111111111100010, 0b11111111111111111100011,
    0b11111111111111111100100, 0b111111111111111011100,
    0b1111111111111111011000, 0b11111111111111111100101,
    0b1111111111111111011001, 0b11111111111111111100110,
    0b11111111111111111100111, 0b111111111111111111101111,
    0b1111111111111111011010, 0b111111111111111011101, 0b11111111111111101001,
    0b1111111111111111011011, 0b1111111111111111011100,
    0b11111111111111111101000, 0b11111111111111111101001,
    0b111111111111111011110, 0b11111111111111111101010,
    0b1111111111111111011101, 0b1111111111111111011110,
    0b111111111111111111110000, 0b111111111111111011111,
    0b1111111111111111011111, 0b11111111111111111101011,
    0b11111111111111111101100, 0b111111111111111100000, 0b111111111111111100001,
    0b1111111111111111100000, 0b111111111111111100010,
    0b11111111111111111101101, 0b1111111111111111100001,
    0b11111111111111111101110, 0b11111111111111111101111,
    0b11111111111111101010, 0b1111111111111111100010, 0b1111111111111111100011,
    0b1111111111111111100100, 0b11111111111111111110000,
    0b1111111111111111100101, 0b1111111111111111100110,
    0b11111111111111111110001, 0b11111111111111111111100000,
    0b11111111111111111111100001, 0b11111111111111101011, 0b1111111111111110001,
    0b1111111111111111100111, 0b11111111111111111110010,
    0b1111111111111111101000, 0b1111111111111111111101100,
    0b11111111111111111111100010, 0b11111111111111111111100011,
    0b11111111111111111111100100, 0b111111111111111111111011110,
    0b111111111111111111111011111, 0b11111111111111111111100101,
    0b111111111111111111110001, 0b1111111111111111111101101,
    0b1111111111111110010, 0b111111111111111100011,
    0b11111111111111111111100110, 0b111111111111111111111100000,
    0b111111111111111111111100001, 0b11111111111111111111100111,
    0b111111111111111111111100010, 0b111111111111111111110010,
    0b111111111111111100100, 0b111111111111111100101,
    0b11111111111111111111101000, 0b11111111111111111111101001,
    0b1111111111111111111111111101, 0b111111111111111111111100011,
    0b111111111111111111111100100, 0b111111111111111111111100101,
    0b11111111111111101100, 0b111111111111111111110011, 0b11111111111111101101,
    0b111111111111111100110, 0b1111111111111111101001, 0b111111111111111100111,
    0b111111111111111101000, 0b11111111111111111110011,
    0b1111111111111111101010, 0b1111111111111111101011,
    0b1111111111111111111101110, 0b1111111111111111111101111,
    0b111111111111111111110100, 0b111111111111111111110101,
    0b11111111111111111111101010, 0b11111111111111111110100,
    0b11111111111111111111101011, 0b111111111111111111111100110,
    0b11111111111111111111101100, 0b11111111111111111111101101,
    0b111111111111111111111100111, 0b111111111111111111111101000,
    0b111111111111111111111101001, 0b111111111111111111111101010,
    0b111111111111111111111101011, 0b1111111111111111111111111110,
    0b111111111111111111111101100, 0b111111111111111111111101101,
    0b111111111111111111111101110, 0b111111111111111111111101111,
    0b111111111111111111111110000, 0b11111111111111111111101110,
    0b111111111111111111111111111111
);
my constant HUFFMAN_LENGTHS = array[int].new(
    13, 23, 28, 28, 28, 28, 28, 28, 28, 24, 30, 28, 28, 30, 28, 28, 28, 28, 28,
    28, 28, 28, 30, 28, 28, 28, 28, 28, 28, 28, 28, 28, 6, 10, 10, 12, 13, 6, 8,
    11, 10, 10, 8, 11, 8, 6, 6, 6, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 8, 15, 6,
    12, 10, 13, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 8, 7, 8, 13, 19, 13, 14, 6, 15, 5, 6, 5, 6, 5, 6, 6, 6, 5, 7, 7, 6, 6,
    6, 5, 6, 7, 6, 5, 5, 6, 7, 7, 7, 7, 7, 15, 11, 14, 13, 28, 20, 22, 20, 20,
    22, 22, 22, 23, 22, 23, 23, 23, 23, 23, 24, 23, 24, 24, 22, 23, 24, 23, 23,
    23, 23, 21, 22, 23, 22, 23, 23, 24, 22, 21, 20, 22, 22, 23, 23, 21, 23, 22,
    22, 24, 21, 22, 23, 23, 21, 21, 22, 21, 23, 22, 23, 23, 20, 22, 22, 22, 23,
    22, 22, 23, 26, 26, 20, 19, 22, 23, 22, 25, 26, 26, 26, 27, 27, 26, 24, 25,
    19, 21, 26, 27, 27, 26, 27, 24, 21, 21, 26, 26, 28, 27, 27, 27, 20, 24, 20,
    21, 22, 21, 21, 23, 22, 22, 25, 25, 24, 24, 26, 23, 26, 27, 26, 26, 27, 27,
    27, 27, 27, 28, 27, 27, 27, 27, 27, 26, 30
);
my constant HUFFMAN_TREE = do {
    my int @tree = 0, 0;
    for 0..256 {
        my int $code = HUFFMAN_CODES[$_];
        my int $i = HUFFMAN_LENGTHS[$_];
        my int $tree-pos = 0;
        while --$i >= 0 {
            my int $tree-index = $tree-pos + ($code +& (1 +< $i) ?? 1 !! 0);
            if $i {
                my int $goto = @tree[$tree-index];
                unless $goto {
                    $goto = @tree.elems;
                    @tree.push(0) xx 2;
                    @tree[$tree-index] = $goto;
                }
                $tree-pos = $goto;
            }
            else {
                @tree[$tree-index] = -$_;
            }
        }
    }
    @tree
};

role X::HTTP::HPACK is Exception { }
class X::HTTP::HPACK::IndexOutOfRange does X::HTTP::HPACK {
    has $.index;
    method message() { "Header table index $!index out of range" }
}
class X::HTTP::HPACK::Overflow does X::HTTP::HPACK {
    has $.what;
    has $.offset;
    method message() { "$!what.tclc() at byte $!offset overflows input buffer" }
}

enum HTTP::HPACK::Indexing <Indexed NotIndexed NeverIndexed>;

class HTTP::HPACK::Header {
    has Str $.name is required;
    has Str $.value is required;
    has HTTP::HPACK::Indexing $.indexing = Indexed;

    multi method new(Pair $_) {
        self.new(name => .key, value => .value)
    }
}

role HTTP::HPACK::Tables {
    my constant NO_VALUE = Mu;
    my constant STATIC_TABLE = [
        Mu,
        ':authority'                => NO_VALUE,
        ':method'                   => 'GET',
        ':method'                   => 'POST',
        ':path'                     => '/',
        ':path'                     => '/index.html',
        ':scheme'                   => 'http',
        ':scheme'                   => 'https',
        ':status'                   => '200',
        ':status'                   => '204',
        ':status'                   => '206',
        ':status'                   => '304',
        ':status'                   => '400',
        ':status'                   => '404',
        ':status'                   => '500',
        accept-charset              => NO_VALUE,
        accept-encoding             => 'gzip, deflate',
        accept-language             => NO_VALUE,
        accept-ranges               => NO_VALUE,
        accept                      => NO_VALUE,
        access-control-allow-origin => NO_VALUE,
        age                         => NO_VALUE,
        allow                       => NO_VALUE,
        authorization               => NO_VALUE,
        cache-control               => NO_VALUE,
        content-disposition         => NO_VALUE,
        content-encoding            => NO_VALUE,
        content-language            => NO_VALUE,
        content-length              => NO_VALUE,
        content-location            => NO_VALUE,
        content-range               => NO_VALUE,
        content-type                => NO_VALUE,
        cookie                      => NO_VALUE,
        date                        => NO_VALUE,
        etag                        => NO_VALUE,
        expect                      => NO_VALUE,
        expires                     => NO_VALUE,
        from                        => NO_VALUE,
        host                        => NO_VALUE,
        if-match                    => NO_VALUE,
        if-modified-since           => NO_VALUE,
        if-none-match               => NO_VALUE,
        if-range                    => NO_VALUE,
        if-unmodified-since         => NO_VALUE,
        last-modified               => NO_VALUE,
        link                        => NO_VALUE,
        location                    => NO_VALUE,
        max-forwards                => NO_VALUE,
        proxy-authenticate          => NO_VALUE,
        proxy-authorization         => NO_VALUE,
        range                       => NO_VALUE,
        referer                     => NO_VALUE,
        refresh                     => NO_VALUE,
        retry-after                 => NO_VALUE,
        server                      => NO_VALUE,
        set-cookie                  => NO_VALUE,
        strict-transport-security   => NO_VALUE,
        transfer-encoding           => NO_VALUE,
        user-agent                  => NO_VALUE,
        vary                        => NO_VALUE,
        via                         => NO_VALUE,
        www-authenticate            => NO_VALUE,
    ];
    my constant STATIC_ELEMS = STATIC_TABLE.elems;

    has @!dynamic-table;
    has Int $.dynamic-table-limit = 4096;

    method set-dynamic-table-limit(Int $new-size) {
        $!dynamic-table-limit = $new-size;
        while self.dynamic-table-size > $!dynamic-table-limit {
            @!dynamic-table.pop;
        }
    }

    method dynamic-table-size(--> Int:D) {
        @!dynamic-table.map({ 32 + .key.chars + .value.chars }).sum
    }

    method !add-to-dynamic-table(Pair $header) {
        @!dynamic-table.unshift($header);
        while self.dynamic-table-size > $!dynamic-table-limit {
            @!dynamic-table.pop;
        }
    }

    method !resolve-decoded-index($index) returns Pair {
        if 0 < $index < STATIC_ELEMS + @!dynamic-table.elems {
            $index < STATIC_ELEMS
                ?? STATIC_TABLE[$index]
                !! @!dynamic-table[$index - STATIC_ELEMS]
        }
        else {
            die X::HTTP::HPACK::IndexOutOfRange.new(:$index);
        }
    }

    method !find-table-entry(HTTP::HPACK::Header $header) {
        my @found = flat(STATIC_TABLE, @!dynamic-table).grep: :p, {
            .defined && .key eq $header.name
        }
        with @found.first({ .value.value andthen $_ eq $header.value }) {
            .key => True
        }
        elsif @found {
            @found[0].key => False
        }
        else {
            Mu
        }
    }
}

class HTTP::HPACK::Decoder does HTTP::HPACK::Tables {
    method decode-headers(Blob $packed) returns Array {
        my @headers;

        my int $idx = 0;
        while $idx < $packed.elems {
            my uint $header-start = $packed[$idx];
            if $header-start +& 128 {
                @headers.push(HTTP::HPACK::Header.new(
                    self!resolve-decoded-index(decode-int($packed, 7, $idx))
                ));
            }
            elsif $header-start +& 64 {
                my $header = self!decode-literal-header-field($packed, 6, $idx);
                @headers.push(HTTP::HPACK::Header.new($header));
                self!add-to-dynamic-table($header);
            }
            elsif $header-start +& 32 {
                given decode-int($packed, 5, $idx) {
                    my $old = $!dynamic-table-limit;
                    $!dynamic-table-limit = $_;
                    unless $old <= $!dynamic-table-limit {
                        while self.dynamic-table-size > $!dynamic-table-limit {
                            @!dynamic-table.pop;
                        }
                    }
                }
            }
            elsif $header-start +& 16 {
                given self!decode-literal-header-field($packed, 4, $idx) {
                    @headers.push(HTTP::HPACK::Header.new(
                        name => .key,
                        value => .value,
                        indexing => HTTP::HPACK::Indexing::NeverIndexed
                    ));
                }
            }
            else {
                given self!decode-literal-header-field($packed, 4, $idx) {
                    @headers.push(HTTP::HPACK::Header.new(
                        name => .key,
                        value => .value,
                        indexing => HTTP::HPACK::Indexing::NotIndexed
                    ));
                }
            }
        }
        @headers
    }

    method !decode-literal-header-field(Blob $packed, $prefix, int $idx is rw) returns Pair {
        my int $header-index = decode-int($packed, $prefix, $idx);
        if $header-index {
            self!resolve-decoded-index($header-index).key => decode-str($packed, $idx)
        }
        else {
            my ($key, $value) = decode-str($packed, $idx) xx 2;
            $key => $value
        }
    }


    sub decode-str(Blob $packed, int $blob-offset is rw) returns Str is export(:internal) {
        my uint $huffman = $packed[$blob-offset] +& 128;
        my uint $bytes = decode-int($packed, 7, $blob-offset);
        my $result-buf;
        if $huffman {
            $result-buf = Buf.new;
            my int $tree-pos = 0;
            my int $end = $blob-offset + $bytes;
            DECODE: while $blob-offset < $end {
                my uint $decode-byte = $packed[$blob-offset++];
                my uint $bit = 128;
                while $bit {
                    my int $node = HUFFMAN_TREE[$tree-pos + ($decode-byte +& $bit ?? 1 !! 0)];
                    if $node > 0 {
                        $tree-pos = $node;
                    }
                    elsif $node < 0 {
                        last DECODE if $node == -256;
                        $result-buf.push(-$node);
                        $tree-pos = 0;
                    }
                    else {
                        die "Invalid huffman code";
                    }
                    $bit +>= 1;
                }
            }
        }
        else {
            die X::HTTP::HPACK::Overflow.new(:what<string>, offset => $blob-offset)
                if $blob-offset + $bytes > $packed.bytes;
            $result-buf = $packed.subbuf($blob-offset, $bytes);
            $blob-offset += $bytes;
        }
        $result-buf.decode('latin-1')
    }
}

class HTTP::HPACK::Encoder does HTTP::HPACK::Tables {
    has Bool $.huffman = False;

    # Implements constraint: `@headers where all(@headers) ~~ HTTP::HPACK::Header` but in a way compatible with the windows all regression.
    method encode-headers(@headers where [and] @headers.map(* ~~ HTTP::HPACK::Header)) returns Blob {
        my $result = Buf.new;
        for @headers -> $header {
            # Search tables for a matching entry.
            my $match = self!find-table-entry($header);

            # If exact match and we're allowed to store indexed, emit indexed header
            # field representation.
            if $match && $match.value && $header.indexing == HTTP::HPACK::Indexing::Indexed {
                encode-int($match.key, 7, $result, 0b10000000);
            }

            # Literal Header Field with Incremental Indexing
            else {
                my $index = $match ?? $match.key !! 0;
                given $header.indexing {
                    when HTTP::HPACK::Indexing::Indexed {
                        encode-int($index, 6, $result, 0b01000000);
                        self!add-to-dynamic-table($header.name => $header.value);
                    }
                    when HTTP::HPACK::Indexing::NotIndexed {
                        encode-int($index, 4, $result);
                    }
                    when HTTP::HPACK::Indexing::NeverIndexed {
                        encode-int($index, 4, $result, 0b00010000);
                    }
                }
                self!encode-str($header.name, $result) unless $match;
                self!encode-str($header.value, $result);
            }
        }
        $result
    }

    method !encode-str(str $value, $target) {
        my $encoded = $value.encode('latin-1');
        if $!huffman {
            my $huffed = Buf.new;
            my uint $cur-byte = 0;
            my int $cur-bit = 8;
            for $encoded.list {
                my int $code = HUFFMAN_CODES[$_];
                my int $code-bit = HUFFMAN_LENGTHS[$_];
                while $code-bit-- {
                    my int $value = $code +& (1 +< $code-bit) ?? 1 !! 0;
                    $cur-byte +|= $value +< --$cur-bit;
                    if $cur-bit == 0 {
                        $huffed.push($cur-byte);
                        $cur-bit = 8;
                        $cur-byte = 0;
                    }
                }
            }
            if $cur-bit < 8 {
                while $cur-bit-- {
                    $cur-byte +|= 1 +< $cur-bit;
                }
                $huffed.push($cur-byte);
            }
            encode-int($huffed.bytes, 7, $target, 0b10000000);
            $target.append($huffed);
        }
        else {
            encode-int($encoded.bytes, 7, $target);
            $target.append($encoded);
        }
    }
}

sub encode-int(uint $value, int $prefix, $target = Buf.new, int $upper = 0)
        returns Buf is export(:internal) {
    my int $limit = 2 ** $prefix - 1;
    if $value < $limit {
        $target.push($value +| $upper);
    }
    else {
        $target.push($limit +| $upper);
        my uint $cur-value = $value - $limit;
        while $cur-value > 128 {
            $target.push($cur-value mod 128 + 128);
            $cur-value = $cur-value div 128;
        }
        $target.push($cur-value);
    }
    $target
}

sub decode-int(Blob $blob, int $prefix, int $blob-offset is rw) returns uint is export(:internal) {
    my int $limit = 2 ** $prefix - 1;
    my uint $result = $blob[$blob-offset] +& $limit;
    if $result >= $limit {
        my int $m = 0;
        my uint $cur-byte = $result;
        repeat while $cur-byte +& 128 {
            $cur-byte = $blob[++$blob-offset];
            $result += ($cur-byte +& 127) * 2 ** $m;
            $m += 7;
        }
    }
    $blob-offset++;
    $result
}

# vim: expandtab shiftwidth=4
