class X::ExportQualifiedKeys::Th is Exception { has $.v; }

# A `sub EXPORT` hash installs its keys, qualified ones included, into the
# importing scope's lexical pad.
sub EXPORT() {
    my %e;
    %e{"X::ExportQualifiedKeys::Th::A"} = X::ExportQualifiedKeys::Th.new(v => 1);
    %e{"X::ExportQualifiedKeys::Th::B"} = X::ExportQualifiedKeys::Th.new(v => 2);
    return %e;
}
