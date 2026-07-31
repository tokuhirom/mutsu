# Cro::Core uri.rakutest 1652/1652 — two regex engine fixes

`uri.rakutest`, the largest file in the vendored Cro::Core suite, went from
1633/1652 to **fully green** with two general regex-engine repairs (both
raku-verified and pinned by new `t/` tests):

## Escaped `\]` inside a `[...]` group closed the group early

The non-capturing-group scanner counted `[`/`]` without honoring backslash
escapes, so Cro::Uri's IPv4address rule
`<.dec-octet> ** 4 % "." [<?[/#?:\]]> || $]` sliced the group at the char
class's escaped `\]`, leaving a stray `>` that failed the whole regex parse
("Unrecognized regex metacharacter >"). Every IPv4 host-class check and all
IPv6-with-embedded-IPv4 parses (`::FFFF:129.144.52.38`, via `ls32` →
`IPv4address`) failed on this one bug — 18 of the 19 remaining failures.
Pin: `t/regex-group-escaped-bracket.t`.

## Zero-match `[...]?` group dropped nested list captures to Nil

When an optional group matches zero times, raku still renders a name under a
nested LIST quantifier as an empty list: `'/' [ <segment-nz> [ '/' <segment>
]* ]?` matching `/` leaves `$<segment>` = `[]` (while `$<segment-nz>`, under
only the `?`, stays Nil). mutsu left both Nil — and since `@$Nil` iterates
once, Cro::Uri's path-absolute action appended a bogus separator:
`parse-ref('/')` produced path `//`, breaking `$base.add('/')` resolution.
The `?` zero paths now mark names found under nested `*`/`+`/`**`/`%`
quantifiers as quantified (empty-list) captures.
Pin: `t/regex-optional-group-list-captures.t`.

With this, the Cro::Core suite stands at: iri 35/35, mediatype 87/87,
message-with-body 6/6, composer 134/134, connection-state 11/11, policy 6/6,
**uri 1652/1652**. Remaining: connection-conditional (state per-clone deep
ticket) and tcp `:nodelay` (in-memory socket fd design ticket).
