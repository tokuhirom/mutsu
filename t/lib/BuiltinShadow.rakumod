unit module BuiltinShadow;

my @calls;

#| Collect what the exported builtin-shadowing routines recorded.
sub shadow-calls(--> Str) is export {
    @calls.join(',')
}

sub shadow-reset(--> Nil) is export {
    @calls = ();
}

#| Run a block from inside this module, the way `Cro::HTTP::Router`'s `route`
#| runs its route-definition block: the bareword calls in the block body are
#| resolved while the module, not the caller, is the current package.
sub shadow-runner(&body --> Str) is export {
    @calls = ();
    body();
    @calls.join(',')
}

# `get` and `lines` are both mutsu builtins that read from a handle. An imported
# routine of the same name must win over the builtin at every call site.
proto sub get(|) is export {*}
multi sub get(&handler --> Nil) {
    @calls.push('get:block');
}
multi sub get(Str $tag, &handler? --> Nil) {
    @calls.push("get:$tag");
}

proto sub lines(|) is export {*}
multi sub lines(&handler --> Nil) {
    @calls.push('lines:block');
}
multi sub lines(Str $tag, &handler? --> Nil) {
    @calls.push("lines:$tag");
}
