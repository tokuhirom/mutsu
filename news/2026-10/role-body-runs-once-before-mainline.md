# A role body runs once when a class composes it after a run-time statement

`say "start"; role R { say "body" }; class A does R { }` printed `body` twice in
mutsu, once from the forward-reference shell and once from the declaration in
place; Rakudo composes the class at BEGIN time and prints it once, before
`start`. A class declaration that composes a role and follows a run-time
statement now joins the unit's BEGIN prologue (ADR-0134), so the body runs once,
over the unit's lexicals in their static state, and `my` bindings the body makes
to outer lexicals keep aliasing the unit's own containers. A declaration with a
user trait (`my %h is R = ...`) is no longer split into a static half and an
assignment, so the trait role's `STORE` still receives the initializer with
`:INITIALIZE`.

Closes #10667.

A scalar bind (`my $x := Proxy.new(...)`) in a unit that has a BEGIN-time effect
after it was split into a static half plus an assignment, which FETCHed the
Proxy into a plain Scalar. It stays whole and binds now.

A native method now takes the FETCHed value of any Proxy argument
(`$blob.subbuf(0, $p)` with a `:=`-bound Proxy `$p`), not only a native
`CArray` element reference; operators on a bound Proxy are tracked in #12782.
