# A bare `Pkg::name` that names an `our &name` calls it

`class Foo { class Bar { our &zape = { "zipi" } } }; say Foo::Bar::zape` died
with "Could not find symbol '&zape' in 'Foo::Bar'" while `Foo::Bar::zape()` and
`Foo::Bar::<&zape>()` worked. The bare qualified term now finds the package's
`our &` variable and calls the routine it holds, as rakudo does (#16482).
