# A multipart/form-data part with no `Content-Type` gets no content-type at all

`Cro::HTTP::Body::MultiPartFormData::Part.content-type` should default to
`text/plain` when the part carries no `Content-Type` header. Under mutsu it is
undefined, so all three assertions fail in `t/http-request-parser.rakutest`:

```
not ok - First part has a content-type that is a Cro::MediaType
not ok - First part has default text type
not ok - First part has default plain subtype
```

while the sibling assertions about the same part (`name`, `body-blob`,
`body-text`) pass. The `is-deeply @parts[0].body, '355…'` assertion fails too,
which is likely the same cause: `Part.body` dispatches on the content-type to
choose a body parser, so an undefined content-type cannot select the text one.

## Why it is newly visible

The multipart parser used to die outright on a grapheme/codepoint index mismatch
(`news/2026-08/string-positions-are-grapheme-based.md`), so every per-part
assertion was skipped. They run now; twelve of them fail on this one issue, and
it is the largest remaining cluster in that file.

## Where to look

`Cro::HTTP::Body::MultiPartFormData::Part` in the Cro::HTTP dist's
`lib/Cro/HTTP/Body.rakumod`, and how `Cro::HTTP::BodyParsers`'
`MultiPartFormData.parse` builds each `Part`. The default is expressed in Cro
itself, so this is very likely a mutsu-side gap in whatever expresses it —
an attribute default, a `content-type` method with a `//` fallback, or role
composition — rather than something to implement natively. Start by reading the
real definition and reproducing it standalone, the way `tmp/multipart.raku`
reproduces the parser.
