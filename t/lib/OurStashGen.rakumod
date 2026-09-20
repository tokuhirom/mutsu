unit class OurStashGen;

# The generated-export idiom, copied in shape from Air::Functional: one sub per
# name, installed into the module's export stash through the `OUR::`
# pseudo-stash with a RUNTIME-computed key, each closing over the loop
# variable. See t/modules/import-export/our-stash-generated-exports.t.

sub wrap(Str $tag, *@inner) is export(:MANDATORY) { "<$tag>{@inner.join}</$tag>" }

my @tags = <aa bb>;

my package EXPORT::DEFAULT {
    for @tags -> $tag {
        OUR::{'&' ~ $tag} := sub (*@inner) { wrap($tag, |@inner) }
    }
    # A literal key, and a non-code symbol, through the same stash.
    OUR::<&cc> := sub { 'cc-called' };
    OUR::<$dd> := 'dd-value';
}
