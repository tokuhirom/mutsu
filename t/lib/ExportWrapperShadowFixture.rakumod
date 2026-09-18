# A `sub EXPORT` that `use`s another module internally, then re-exports one
# of its `is export` subs wrapped in a closure UNDER THE SAME BARE NAME
# (#8746). The bare name is ambiguous after this: the registry still holds
# the inner module's own, unwrapped routine under the same flat name, while
# `env` holds the wrapper. A bareword call of the exported name from the
# importer must run the wrapper, not the routine it wraps.
sub EXPORT(--> Map()) {
    use ExportWrapperShadowInnerFixture;
    '&export-wrapper-shadow-greet' => -> |c {
        export-wrapper-shadow-greet(|c, :from<wrapped>);
    },
}
unit module ExportWrapperShadowFixture;
