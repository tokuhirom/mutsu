Builtin encoding objects now use the same codecs and buffer construction as
`Str.encode`. UTF-16 encoders return UTF-16 code units or bytes with the
requested endianness, and single-byte encoders no longer emit UTF-8 bytes.
Replacement text is encoded and checked in the target codec; a false
replacement keeps strict encoding.
