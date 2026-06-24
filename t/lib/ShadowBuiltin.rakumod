unit module ShadowBuiltin;

# These names collide with core builtins (`get` reads a line from an IO::Handle,
# `close` closes a handle). An importing scope must see THESE subs, fully
# shadowing the builtins.
sub get(Str $path, &handler) is export { return "GET $path -> " ~ handler() }
sub close(Str $what)         is export { return "closed $what" }
