# Propagate regex subrule parameter type errors

Named regex and token subrules now report parameter binding errors when no candidate can accept an argument. Anonymous Regex values used as subrules do the same. A compatible multi candidate can still match when another candidate rejects the argument type.

Each grammar parse starts with a fresh pending regex error. A failed parse can no longer leave a speculative subrule binding error that breaks the next parse, as the YAMLish battery test exposed.
