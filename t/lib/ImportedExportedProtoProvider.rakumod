unit module ImportedExportedProtoProvider;

proto sub imported-exported-calendar(|) is export {*}
multi sub imported-exported-calendar($months = Whatever) {
    "provider"
}

proto sub imported-exported-calendar-year(|) is export {*}
multi sub imported-exported-calendar-year($year = Whatever) {
    "provider-year"
}
