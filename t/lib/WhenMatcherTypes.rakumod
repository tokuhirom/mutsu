unit module WhenMatcherTypes;

class Outer {
    class Inner {
        has $.tag = "inner";
    }
}

role Marker {
}

enum Colour <Red Green Blue>;
