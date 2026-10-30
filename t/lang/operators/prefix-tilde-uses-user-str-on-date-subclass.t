use Test;

plan 4;

# `~$obj` is `.Str`: a subclass of a built-in temporal class keeps its own.
class MyDate is Date {
    method Str { "my-date" }
}
class MyDateTime is DateTime {
    method Str { callsame.subst: 'Z' }
}

is ~MyDate.new("2020-01-02"), "my-date", 'prefix ~ on a Date subclass calls the user Str';
is ~MyDateTime.new("1987-07-05T17:45:00"), "1987-07-05T17:45:00", 'a DateTime subclass strips the Z';
is MyDate.new("2020-01-02").Str, "my-date", 'the explicit .Str agrees';
is (MyDateTime.new("1987-07-05T17:45:00") ~ "!"), "1987-07-05T17:45:00!", 'infix ~ agrees';
