# A script's multi no longer joins a module's same-named family

A `multi sub` declared in the main script and an unexported `multi sub` of
the same name in a package-less module are two separate lexical families in
Raku. mutsu registered both under the same `GLOBAL::` keys, and the script's
candidates had no visibility record. So inside the module, a call could
dispatch to the script's candidate (#11081).

Once a module's family of a name is scoped to its compunit (#11004), the
script's own family of that name is now scoped to the main script as well.
Each side then dispatches, lists `.candidates`, and reports
`X::Multi::NoMatch` signatures from its own family only. Along the way a TRIR
routine body now runs in its declaring compilation unit, as the other call
entries already did.
