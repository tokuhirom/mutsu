# A package-qualified bare name of an `our &` variable is a call

`Foo::Bar::zape` with no parentheses, where `zape` is an `our &zape` code
variable, died "Could not find symbol '&zape' in 'Foo::Bar'" although
`Foo::Bar::zape()` and `Foo::Bar::<&zape>()` worked. The bare qualified term now
looks the `&Foo::Bar::zape` stash entry up and calls the Callable, as it does for
an `our sub`.
