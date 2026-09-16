unit module ProtoExportTagFixture;

# A `proto` whose `is export` carries an explicit tag, with `multi` candidates
# that carry no `is export` of their own — the FunctionalParsers distribution
# shape (its `sequence`/`alternatives` combinators): the proto is the only
# declaration that names the tag, and the whole multi family is exported
# through it.
proto sub combine(|) is export(:mine, :ALL) {*}

multi sub combine($a) { $a }

multi sub combine($a, $b) { $a + $b }
