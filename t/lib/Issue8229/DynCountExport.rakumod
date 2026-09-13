sub EXPORT (|) {
    $*PACKAGE_LOADED++;
    BEGIN Map.new
}
