# Reduced from the `Exportable` distribution (a dependency of File::Stat and
# Color::DirColors): the trait handlers are declared `is export` inside
# `sub EXPORT`, close over its `%exports`, and re-dispatch to CORE's
# `trait_mod:<is>(Routine, :$export!)`; the returned `&EXPORT` becomes the
# importing module's own EXPORT.
sub exported-EXPORT(%exports, *@names --> Hash()) {
    do for @names -> $name {
        unless %exports{ $name }:exists {
            die("Unknown name for export: '$name'");
        }
        "&$name" => %exports{ $name }
    }
}

sub EXPORT {
    my %exports;
    multi sub trait_mod:<is>(Routine:D \r, Bool :$exportable!) is export {
        trait_mod:<is>(r, :exportable(r.name => True));
    }
    multi sub trait_mod:<is>(Routine:D \r, :$exportable!) is export {
        trait_mod:<is>(r, :export($exportable));
        %exports{ r.name } = r
    }
    {
        '&EXPORT' => sub (*@names) { exported-EXPORT %exports, |@names }
    }
}
