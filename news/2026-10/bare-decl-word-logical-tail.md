# A loose word-logical after a bare declaration parses

`my %h andthen do { ... }` and `my $x or ...` died at compile time with
`Confused. Two terms in a row`. The loose word-logicals (`and`, `or`, `xor`,
`andthen`, `orelse`, `notandthen`) are looser than a declaration. An
initialized declaration (`my $x = 1 and ...`) already had its trailing
word-logical re-attached to the declared variable. A declaration with no
initializer now gets the same treatment, so `my %h andthen do { ... }` runs as
`(my %h) andthen do { ... }`.

Found via the `WebService::Slack::Webhook` distribution, which could not load
because its dependency `Net::HTTP` parses response headers with
`my %header andthen do { ... for @header-lines>>.split(':', 2) }`. All three of
its test files now pass under mutsu. The group form `my ($a, $b) andthen ...`
is filed as #11330.
