# A module's multi candidate keeps the module's own `my` statics

`multi sub` candidates imported from a module run as captured candidates of the importing scope's
dispatcher value. Their free `my %seen` / `my $total` names resolve through the call chain before
the capture, so a calling *method* that injected a same-named class static (`my %seen{Hash}` at
its class's file scope) answered instead of the module's own. The candidate call now injects the
declaring package's statics the way the named-call entry does, and puts the caller's bindings back
afterwards. `PDF::COS::Util`'s `ast-coerce` cycle guard (`%seen`) no longer recurses forever, so
PDF's `t/io-serialize.t` gets past its first six tests (#14600).
