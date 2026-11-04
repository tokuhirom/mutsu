# A rule ending in a separated quantifier returns ratcheted

`rule e { <item>+ % ','? }` left its quantifier backtrackable because the
rule's trailing whitespace became a `<.ws>` after the separator, and the
separator pass marked the quantifier `:!` for "a later term may need an item
back". Nothing follows inside the rule there, so a caller that failed after
`<e>` was offered every shorter list and the rule's action ran again for each.
`rakudo` returns the one ratcheted match.

The separator pass now marks the quantifier only when something follows the
`<.ws>`, the same exemption the plain-term pass already had. Found with
CSS::Grammar, whose `t/compat.t` declarations case emitted its "unknown units"
warnings once per extra pass; one pass remains (a rule with an internal
backtrackable term still hands its caller several ends, filed separately).
