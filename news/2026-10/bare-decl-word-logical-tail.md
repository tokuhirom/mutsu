# Bare declarations accept a trailing word-logical; `notandthen` topicalizes

A declaration without an initializer followed by a loose word-logical —
`my %header andthen do { ... }`, `my $x orelse fail` — now parses as
`(my %header) andthen ...`, the same way `my $x = 1 and 2` already did.
mutsu used to stop after the declaration and report "Confused. Two terms in
a row". The construct sits in Net::HTTP's `Net/HTTP/Transport.rakumod`, so it
blocked every distribution depending on Net::HTTP (raku-mailgun's `use Mailgun`
now loads, and its test file passes).

While checking the forms, `notandthen` was found to leave the outer `$_` in
place while evaluating its right operand; it now binds the undefined left
operand to the topic, like `orelse` (`Int notandthen $_.raku` is `"Int"`).
