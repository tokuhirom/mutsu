use Test;

plan 5;

# A package-qualified name with no parentheses that names an `our &` code
# variable is a call of the stored Callable, as it is for an `our sub` (#16482).

class Foo {
    class Bar {
        our &zape = { "zipi" };
        our sub named { "named" }
    }
}

is Foo::Bar::zape, "zipi", 'bare qualified name of an our & variable calls it';
is Foo::Bar::zape(), "zipi", 'the parenthesised call still works';
is Foo::Bar::<&zape>(), "zipi", 'the stash lookup form still works';
is Foo::Bar::named, "named", 'an our sub bare call still works';
throws-like { (Foo::Bar::zape)() }, X::Method::NotFound,
    'the bare name is a call, so calling its string result dies';
