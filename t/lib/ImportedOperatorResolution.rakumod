# Fixture for t/rakuast/rakuast-imported-operator-resolution.t: a module that
# exports candidates of a core infix and a core postfix, as Red::Operators does.
unit module ImportedOperatorResolution;
class OpTag is export { }
multi infix:<==>(OpTag $a, Numeric() $b) is export { "tag" }
multi postfix:<++>(OpTag $a) is export { "inc" }
