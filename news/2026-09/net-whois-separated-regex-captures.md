# Net::Whois IP subset validation sees captures from separated quantifiers

`Net::Whois` 0.0.3's `IP` subset uses an assertion inside a separated
quantifier to reject octets greater than 255. mutsu evaluated that assertion
with only the current atom's captures, so `192.168.1.256` and
`192.168.1.999` were accepted even though Rakudo rejects them. The
interpreter now exposes the accumulated captures from earlier atoms while
evaluating inline regex code, preserving the inline match's own span for
operations such as `$/` and `make`.

The regression is covered by
`t/regex/match/regex-separated-quantifier-code-assertion-captures.t`, and the
roulette measurement moves Net::Whois from partial (3/4 files, 14/15
assertions) to green (4/4 files, 15/15 assertions).
