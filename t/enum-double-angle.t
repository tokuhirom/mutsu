use Test;

plan 9;

# Basic << >> enum with colonpair values
{
    enum Negation << :isnt<isnt> :arent<arent> :amnot<amnot> :aint<aint> >>;
    my Negation $foo;
    lives-ok { $foo = Negation::isnt }, 'can assign enum value from << >> enum';
    is $foo, Negation::isnt, 'assigned value matches';
    is $foo.value, 'isnt', 'colonpair value preserved';
    is Negation::arent.value, 'arent', 'second colonpair value';
}

# << >> enum with plain words (auto-numbered)
{
    enum Colors << red green blue >>;
    is Colors::red.value, 0, 'plain word enum starts at 0';
    is Colors::green.value, 1, 'second plain word';
    is Colors::blue.value, 2, 'third plain word';
}

# << >> enum with multiline
{
    enum Directions <<
        north
        south
        east
        west
    >>;
    is Directions::north.value, 0, 'multiline << >> enum first value';
    is Directions::west.value, 3, 'multiline << >> enum last value';
}
