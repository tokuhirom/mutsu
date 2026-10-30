# Set comparison operators a distribution (Red) overloads for its own type.
unit module SetCompareUserInfix;

class Col is export { has $.name }

multi infix:«(<=)»(Col $a, Positional $b) is export { "le" }
multi infix:<⊆>(Col $a, Positional $b) is export { "le-u" }
multi infix:<⊇>(Col $a, Positional $b) is export { "ge-u" }
multi infix:«(<)»(Col $a, Positional $b) is export { "lt" }
multi infix:<⊂>(Col $a, Positional $b) is export { "lt-u" }
multi infix:«(>)»(Col $a, Positional $b) is export { "gt" }
multi infix:<⊃>(Col $a, Positional $b) is export { "gt-u" }
