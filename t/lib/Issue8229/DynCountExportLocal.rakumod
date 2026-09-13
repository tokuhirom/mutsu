sub EXPORT (|) {
    $*PACKAGE_LOADED++;
    my $export-local = 'from-export';
    BEGIN Map.new
}
