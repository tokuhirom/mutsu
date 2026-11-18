# Fixture for t/modules/import-export/need-module-name-selectors.t: a module
# that needs another with a version selector on its name (Compress::Zlib).
unit module NeedSel;
need NeedSel::Raw:ver<1.1.0+>;
our sub answer { NeedSel::Raw::raw-answer() }
