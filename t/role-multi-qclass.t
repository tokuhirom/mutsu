use Test;

plan 1;

throws-like q:to/END/, X::Role::Unimplemented::Multi,
    'multi role method using ::?CLASS fails with X::Role::Unimplemented::Multi';
my role R {
    multi method m(::?CLASS:D: --> ::?CLASS) { self }
}
my class Foo does R { }
END
