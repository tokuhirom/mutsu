# Fixture for t/modules/import-export/our-dynamic-export-bound-by-importer.t:
# a module that loads OurDynExport before the test file does.
use OurDynExport;
sub our-dyn-mid-get() is export { OurDynReader.new.get }
