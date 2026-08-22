unit module WhenMatcherTypes;

class Outer {
    class Inner {
        has $.tag = "inner";
    }
}

role Marker {
}

enum Colour <Red Green Blue>;

# A `constant ... is export` is a complete nullary term wherever the importer
# can see it, so a bareword `when MATCHER_CONSTANT { }` is never the routine
# call the gobbled-block check looks for. DBDish::Oracle::StatementHandle's
# `when SQLT_NUM { }` is the real-world case: the constant lives in a sibling
# file of the same distribution.
constant MATCHER_CONSTANT is export = 7;
