# Preserve composite subrule arguments in regex code blocks

Regex subrules now bake a parseable Raku representation for composite bound
parameters when preparing embedded code blocks. A block-literal argument such
as a hash containing an angle-word list therefore reaches the subrule's code
block instead of leaving its parameter unresolved and failing the match.
