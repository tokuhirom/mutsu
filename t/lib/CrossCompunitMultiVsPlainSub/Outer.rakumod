use Inner;

# A same-named PLAIN (non-multi) exported sub in the module that USES Inner.
# `Inner`'s own bare `to-toml(...)` calls must never resolve to this one --
# `Inner` never `use`s `Outer`, so per-compunit lexical scoping must keep this
# invisible to it, however both wind up sharing a registry base-name bucket.
sub to-toml(Associative:D $container --> Str:D) is export {
    render($container);
}

sub to-toml-list(@l --> Str:D) is export {
    render-list(@l);
}
