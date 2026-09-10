use Test;

plan 1;

# multi method with ::?CLASS invocant in a role should compose fine
# (raku accepts this without error)
lives-ok {
    EVAL q:to/END/;
my role R {
    multi method m(::?CLASS:D: --> ::?CLASS) { self }
}
my class Foo does R { }
END
}, 'multi role method using ::?CLASS composes without error';
