# `.encode` accepts U+FEFF in the encoding_rs codecs

`"\x[FEFF]A".encode('utf8')` died with "Error encoding UTF8 string: could not encode codepoint
65279". The per-character "is it encodable" round-trip decoded the encoded bytes with
`encoding_rs::Encoding::decode`, which strips a leading BOM, so U+FEFF never round-tripped. It
now uses `decode_without_bom_handling`. Property::Text::Controls' `t/utf8-offsets.rakutest` now
matches raku (26/26); its `t/contracts.rakutest` reaches the end under mutsu as well.
