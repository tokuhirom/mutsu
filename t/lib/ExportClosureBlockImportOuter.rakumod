# `sub EXPORT` whose closures call routines its own block-scoped `use`
# imported (#8751). One wrapper has a different name from the routine it
# calls, the other re-exports under the SAME name.
sub EXPORT(--> Map()) {
    use ExportClosureBlockImportInner;
    %(
        '&ecbi-wrapped' => -> |c { ecbi-real(|c, :from<wrapped>) },
        '&ecbi-greet'   => -> |c { ecbi-greet(|c, :from<same-name wrapper>) },
    )
}
unit module ExportClosureBlockImportOuter;
