# Fixture for t/modules/import-export/our-dynamic-export-bound-by-importer.t:
# a `unit module` exporting an `our` dynamic variable that its own class reads.
unit module OurDynExport;
our $*TROOT is export;
class OurDynReader is export {
    method get() { my $o = $*TROOT // 'unset'; $o }
}
