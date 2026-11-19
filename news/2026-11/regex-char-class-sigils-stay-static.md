# Sigils inside a regex character class no longer make the pattern dynamic

`regex_pattern_is_static` decides whether a regex can be parsed once and served from the parse
cache, or must be re-interpolated on every match. It treated the `@!` of a character class such as
`<[A..Za..z0..9._~:@!$&'()*+,;=-]>` as an `@!attr` interpolation. Every member of a class is a
literal, so that pattern is in fact static. Cro::Uri's `pchars` and `segment-nz-nc` tokens have
exactly this shape. Each URI parse therefore re-ran `interpolate_regex_scalars` on them, about five
times per parse at ~17k instructions each.

The predicate now skips a class body (`<[...]>`, `<-[...]>`, `<+[...]>`, `<?[...]>`, `<![...]>`) up
to its first unescaped `]`. The skip starts only outside quotes and code blocks, where `<[` cannot
be anything else, so a variable after the class or inside a quoted `"<["` is still seen.

`Cro::Uri.parse-ref` went from about 532k to 407k instructions per call (callgrind, release
build). MCP::Server::Tool::Web's `extract-links` over its 1 MB test fixture runs about 6% faster.
