# RakuAST knows the names a loaded module makes visible

Rakudo resolves a bare or qualified name while it parses, so its RakuAST
node says what the name is. mutsu's converter learns the names of a `use`d
module from the parse-time module scan. That scan missed several kinds, so
`.AST` rendered them as routine calls, or refused them, and the
`MUTSU_RAKUAST=1` round trip then failed with `Could not find symbol '&New'`:

```raku
use DepsShort::Dispatcher;      # which itself uses DepsShort::LC
say DepsShort::LC::New;         # an enum value qualified by its enum's name
```

The scan now also records an `our` enum's values qualified by the enum's
name. It records the short name of a qualified enum or role exported with
`is export` (a class already got one). It records the `EXPORT`,
`EXPORT::DEFAULT` and `EXPORT::ALL` packages an `is export` routine creates,
and a value bound straight into an export stash
(`BEGIN EXPORT::special::<Answer> := 42`). An `import Module :TAG` now
registers the enum values its tags select, as `use Module :TAG` did.

A constant bound to a package the same file declares,
`constant E = Outer::Inner`, now renders as that type, and the names
declared under the package are known through it (`E::Status::Started`),
as in rakudo.

Eighteen more `t/` files pass under `MUTSU_RAKUAST=1`. Nine of them were
already passing but were not listed.
