# A module's own type beats an importer's same-named constant

A module body runs against its importer's scope in mutsu, so a constant the
importer declares under the name of one of the module's types
(`use Reg; constant SpecRegistry = Reg::SpecRegistry;`) answered the
module's own `SpecRegistry` bareword. While `use Reg` loaded, that constant
was still the unassigned placeholder, so `my SpecRegistry $REGISTRY =
SpecRegistry.new` in the module built an `Any`, and BDD::Behave's
`t/registry/query-filters.rakutest` died with "No such method 'suites' for
invocant of type 'Any'" (#13628).

A bareword read inside a package now resolves to the type that package (or
an enclosing one) declares ahead of a sigil-less term the running code did
not bind itself. A term declared in the running frame, the running module's
own file-scope symbols and its imports keep outranking the type, as before.
