# A routine's `my enum` no longer copies the registry on every thread

mutsu registered a `my enum` when its declaration ran, i.e. every time the
enclosing routine or supply body ran. A thread clone shares its parent's
declaration registry and type tables copy-on-write, and its parent had usually
never run the body, so the first run on each clone wrote the enum and copied
the whole registry into the clone. Cro's request/response parsers declare
`my enum`s in their supply bodies, so every keep-alive connection paid this.

```raku
sub f() { my enum Expecting <RequestLine Header Body>; RequestLine }
await (^50).map: { start { f() } };
# MUTSU_VM_STATS: registry-cow clones 51 -> 1, program-table-cow 102 -> 2
```

The unit's load now registers the shared part of each auto-numbered `my enum`
below its top level (its variants, `my` scoping, language revision and class
scoping) once, for programs and modules alike, so a later run on any thread
finds it already there and writes nothing shared (#13010).
