# Enum members in attribute defaults resolve inside the declaring module

`has Period $.period = keep;` in a module evaluated `keep` against the running
routine's file, so constructing the class from a method of another file turned
the enum member into a bare string and failed the type check. Attribute defaults
now resolve enum members in the declaring unit. Found with
Date::Calendar::Gregorian (t/03-accessors.rakutest).
