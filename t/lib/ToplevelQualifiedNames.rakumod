unit module ToplevelQualifiedNames;

our enum Color <Red Green Blue>;
enum Shade is export <Light Dark>;
our subset Small of Int where * < 10;

our class Thing {
    method short-enum { Color::Green }
    method long-enum  { ToplevelQualifiedNames::Color::Blue }
    method pkg-enum   { ToplevelQualifiedNames::Red }
    method small      { 3 ~~ ToplevelQualifiedNames::Small }
}

our sub direct() {
    (Red, Color::Green, ToplevelQualifiedNames::Blue,
     ToplevelQualifiedNames::Color::Red, Dark).map(*.Str).join(',')
}
