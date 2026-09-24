unit module ImportedExportedProtoWrapper;

use ImportedExportedProtoProvider;

# A local multi extends an imported exported proto family. The local candidate
# is exported with the provider's family even without its own `is export`.
multi sub imported-exported-calendar(**@args,
                                     *%args where (%args<format> // 'None') eq 'html') {
    "wrapper"
}

multi sub imported-exported-calendar-year(**@args,
                                          *%args where (%args<format> // 'None') eq 'html') {
    "wrapper-year"
}
