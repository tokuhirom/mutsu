# Propagate regex subrule parameter type errors

Named regex and token subrules now report parameter binding errors when no candidate can accept an argument. Anonymous Regex values used as subrules do the same. A compatible multi candidate can still match when another candidate rejects the argument type.
