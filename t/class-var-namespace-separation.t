use Test;

plan 8;

# A $-sigiled variable with the same base name as a class should not
# shadow the class type object.
{
    my $Foo;
    class Foo { method bar { $Foo++ } };
    is Foo.^name, 'Foo', 'class Foo accessible before $Foo modification';
    Foo.new.bar;
    is Foo.^name, 'Foo', 'class Foo still accessible after $Foo++';
    Foo.new.bar;
    is $Foo, 2, '$Foo was incremented correctly';
    is Foo.^name, 'Foo', 'class Foo still accessible after second $Foo++';
}

# Sigilless variables should still shadow bare words
{
    my \Val = 42;
    is Val, 42, 'sigilless variable Val resolves via bare word';
}

# Constants should resolve via bare word
{
    constant Limit = 100;
    is Limit, 100, 'constant Limit resolves via bare word';
}

# $-sigiled variable should not interfere with class used as type
{
    my $Int = "hello";
    is Int.^name, 'Int', 'builtin type Int not shadowed by $Int';
    is $Int, "hello", '$Int retains its value';
}
