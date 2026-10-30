# A frugal tail after a `~` goal keeps growing in a token

`token { "'''" ~ "'''" .*? }` could not match: under ratchet the compiled `~`
committed the inner pattern (`.*?`, at zero length) before testing the goal.
rakudo lets a frugal loop keep growing until what follows matches, as mutsu
already does for `.*? "'''"`; the commit is now skipped when the inner pattern
ends in a frugal quantifier. Found with JSON::Hjson, whose multiline strings use
this form; its `t/01-basic.t` and `t/02-testcases.t` now match raku.
