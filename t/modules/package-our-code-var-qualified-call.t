use Test;

plan 5;

# A bare package-qualified name that names an `our &code` variable is a call of
# the routine it holds, like the parenthesised form (#16482).
class Foo {
    class Bar {
        our &zape = { "zipi" };
        our sub named { "named" }
    }
}

is Foo::Bar::zape, 'zipi', 'bare qualified name calls the our & variable';
is Foo::Bar::zape(), 'zipi', 'the parenthesised form';
is Foo::Bar::<&zape>(), 'zipi', 'the stash form';
is Foo::Bar::named, 'named', 'an our sub (unchanged)';
my $x = Foo::Bar::zape;
is $x, 'zipi', 'in an initializer';
