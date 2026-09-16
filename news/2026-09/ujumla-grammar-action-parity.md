# Ujumla grammar-action parity is tracked as a deep interpreter finding

The ecosystem parity run for Ujumla 0.0.5 was remeasured on current mutsu and
remains partial: 2 of 8 baseline files pass. Rakudo passes all eight baseline
files.

Ujumla's ordinary user grammar uses repeated named captures and an `Actions`
class that converts `$/.made` values into configuration objects while retaining
an interpolation Hash. Mutsu loses that action/capture state across nested
rules, first surfacing as `Type Array does not support associative indexing.`
The ensuing state divergence also accounts for the here-doc, interpolation,
and include failures.

The work is recorded as [#8561](https://github.com/tokuhirom/mutsu/issues/8561)
because matching that protocol is a cross-cutting grammar/regex invariant, not
a bounded module-specific fix.
