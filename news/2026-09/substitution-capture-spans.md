# Preserve capture positions after substitution

Regex substitutions now retain each capture's position in the original string when building the Match objects left in `$/`. This covers string replacements through `.subst` and the `s///` operator, including later matches in a global substitution. Global results also keep the itemized List shape reported by Rakudo.
