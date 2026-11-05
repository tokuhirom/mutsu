use Test;

plan 11;

# A core class whose `new` has typed positional parameters autothreads over a
# Junction argument; the collection constructors store it instead.
is Date.new(2022, 1|2, 5).raku, 'any(Date.new(2022,1,5), Date.new(2022,2,5))',
    'Date.new threads a month junction';
is Date.new(2022, any(1..3), 5).raku,
    'any(Date.new(2022,1,5), Date.new(2022,2,5), Date.new(2022,3,5))', 'an any() of three';
is Date.new(2022, 1|2, *).raku, 'any(Date.new(2022,1,31), Date.new(2022,2,28))',
    'a Whatever day beside a junction month';
is Date.new("2022-01-05"|"2022-02-05").raku, 'any(Date.new(2022,1,5), Date.new(2022,2,5))',
    'a string junction';
is DateTime.new(2022, 1|2, 5, 0, 0, 0).raku,
    'any(DateTime.new(2022,1,5,0,0,0), DateTime.new(2022,2,5,0,0,0))', 'DateTime.new';
is Rat.new(1|2, 3).raku, 'any(<1/3>, <2/3>)', 'Rat.new';
is Complex.new(1|2, 3).raku, 'any(<1+3i>, <2+3i>)', 'Complex.new';
is Version.new("1"|"2").raku, 'any(v1, v2)', 'Version.new';
is Set.new(1|2).raku, 'Set.new(any(1, 2))', 'Set.new stores the junction';
is Array.new(1|2).raku, '[any(1, 2)]', 'Array.new stores the junction';
is Date.new(2022, 1, 5).raku, 'Date.new(2022,1,5)', 'a plain call is unchanged';
