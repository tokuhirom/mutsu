## Base64

Lazy base64 encoding and decoding routines

## Synopsis

    use Base64;


    # ENCODING

    > say encode-base64("g-g-g-g-g unit", :str);
    Zy1nLWctZy1nIHVuaXQ=

    > say encode-base64("g-g-g-g-g unit").raku;
    ("Z", "y", "1", "n", "L", "W", "c", "t", "Z", "y", "1", "n", "I", "H", "V", "u", "a", "X", "Q", "=").Seq

    > .print for lazy encode-base64("g-g-g-g-g unit")
    Zy1nLWctZy1nIHVuaXQ=


    # DECODING

    > say decode-base64("Zy1nLWctZy1nIHVuaXQ=", :bin).decode
    g-g-g-g-g unit

    > say decode-base64("Zy1nLWctZy1nIHVuaXQ=", :bin)
    Buf:0x<67 2d 67 2d 67 2d 67 2d 67 20 75 6e 69 74>

    > say decode-base64("Zy1nLWctZy1nIHVuaXQ=").raku
    (103, 45, 103, 45, 103, 45, 103, 45, 103, 32, 117, 110, 105, 116).Seq

## Exports

#### encode-base64($encode-me where Blob|Str, :$pad where Bool:D|Str:D, Str:D :@alpha, Bool:D :$str --> Seq|Str)

    encode-base64($encode-me)                  # Returns a base64 encoded string
    encode-base64($encode-me, :str)            # Stringify and return the sequence that would be returned
    encode-base64($encode-me, :!pad)           # No padding
    encode-base64($encode-me, :pad("*"))       # Alternative padding character
    encode-base64($encode-me, :uri)            # Use '-' and '_' for chars 63 and 64
    encode-base64($encode-me, :alpha(1..64))   # Set the entire alphabet

Takes a `Blob` and applies base64 encoding with the requested options. If passed a `Str` it will be converted to a `Blob` via `.ords` first.

    > say encode-base64("test", :str)
    dGVzdA==

    > say encode-base64(Buf.new("test".ords), :str)
    dGVzdA==

    > say encode-base64("test").raku'
    ("d", "G", "V", "z", "d", "A", "=", "=").Seq

#### decode-base64($decode-me where Blob:D|Str:D, Str:D :@alpha, Bool:D :$bin, Bool:D :$uri --> Seq|Buf)

    decode-base64($decode-me)                  # Decodes a base64 encoded string to a Buf for further decoding
    decode-base64($encode-me, :bin)            # Return a buffer from the sequence that would be returned
    decode-base64($decode-me, :uri)            # Use '-' and '_' for chars 63 and 64
    decode-base64($decode-me, :alpha(1..64))   # Set the entire alphabet

Takes a `Str` and applies base64 decoding with the requested options. If passed a `Blob` it will be converted to a `Str` with `.decode` first.

    > say decode-base64("YW55IGNhcm5hbCBwbGVhc3VyZS4=", :bin).decode
    any carnal pleasure.

    > say Buf.new(decode-base64("YW55IGNhcm5hbCBwbGVhc3VyZS4=")).decode('utf-8')
    any carnal pleasure.

    > say decode-base64("YW55IGNhcm5hbCBwbGVhc3VyZS4=").raku
    (97, 110, 121, 32, 99, 97, 114, 110, 97, 108, 32, 112, 108, 101, 97, 115, 117, 114, 101, 46).Seq
