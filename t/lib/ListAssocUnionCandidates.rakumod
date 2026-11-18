# Candidates of the built-in `infix:<(|)>` for Ranges, shaped like the
# Range::SetOps distribution's: a binary candidate and a list-associative
# slurpy one (t/modules/import-export/imported-builtin-op-list-assoc-chain.t).
unit module ListAssocUnionCandidates;

multi sub infix:<(|)>(Range:D $a, Range:D $b) is export { "pair({$a.min},{$b.min})" }

multi sub infix:<(|)>(**@r where { $_.all ~~ Range }) is assoc<list> is export {
    "list(" ~ @r.map(*.min).join(",") ~ ")"
}
