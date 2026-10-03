# A loose word-logical after a bare group declaration parses

`my ($a, $b) andthen say "y";` used to die with "Confused. Two terms in a row". The
group-declaration path without an initializer now re-attaches a trailing
`and`/`or`/`xor`/`andthen`/`orelse`/`notandthen`, seeded with the declared variables as a
list, as the single-variable form already did (#11330).
