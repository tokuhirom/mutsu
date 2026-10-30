# `.clone` no longer shares an `@`/`%` attribute nobody has filled

Data::Dump::Tree makes `my $clone = self.clone(|args.hash)` "so we do not pollute the object", then fills
`@.kb_colors` in the clone. An `@`/`%` attribute that was never filled is not vivified in Rakudo, so the
clone and the original each get their own container on first use; a filled one stays shared (the
existing container-identity behavior). mutsu always shared the container, so the clone's fill showed up
on the original and the dumper's dump of itself grew eleven lines (`t/71_filter.rakutest`: 56 lines, got
67). An empty, non-itemized Array/Hash attribute is now copied by `clone`; a scalar holding an array
(itemized) keeps sharing it.
